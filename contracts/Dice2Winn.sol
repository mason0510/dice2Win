pragma solidity ^0.4.24;
// * dice2.win
//-第五版本公平的赌博游戏
// * 合约地址：0xD1CEeeeee83F8bCF3BEDad437202b6154E9F5405.
//随机数的产生是由区块哈希 和网站生成的随机数 混合而来 能免除来自矿工和庄家 玩家的骚扰，不但是完全透明的 并且能够允许较高的赌注。
// * 白皮书  https://dice2.win/whitepaper.pdf for detailed description and proofs.

//骰子合同
contract Dice2Win {
    /// *** Constants section 每一个赌注有百分之分配给庄家 交易价格最高10Gwei
    uint constant HOUSE_EDGE_PERCENT = 1;//庄家赢面1
    uint constant HOUSE_EDGE_MINIMUM_AMOUNT = 0.0003 ether;//最小额度

    uint constant MIN_JACKPOT_BET = 0.1 ether;//最少赌注 少于不会归集到奖池里面

    // 统计奖金时将其扣除
    uint constant JACKPOT_MODULO = 1000;//头等奖
    uint constant JACKPOT_FEE = 0.001 ether;//费用

    // 最小投注和最大投注
    uint constant MIN_BET = 0.01 ether;//最小赌注
    uint constant MAX_AMOUNT = 300000 ether;//最大赌注

    // Modulo is a number of equiprobable outcomes in a game: 不同的游戏模 不同
    //  - 2 for coin flip
    //  - 6 for dice
    //  - 6*6 = 36 for double dice
    //  - 100 for etheroll
    //  - 37 for roulette
    //  etc.
    // It's called so because 256-bit entropy is treated like a huge integer and
    // the remainder of its division by modulo is considered bet outcome.
    //modulo 最大值100 最小值2  不同的值代表不同的游戏 256位的散列数(sea256加密生成)通常被认为是安全的，除以模的余额可以当作结果。
    uint constant MAX_MODULO = 100;

        //低于100进行商值检查
    // For modulos below this threshold rolls are checked against a bit mask,
    // thus allowing betting on any combination of outcomes.
    //mask(base-2,big )
    //For example, given
    // modulo 6 for dice, 101000 mask (base-2, big endian) means betting on
    // 4 and 6; for games with modulos higher than threshold (Etheroll), a simple
    // limit is used, allowing betting on any outcome in [0, N) range.
    //
    // The specific value is dictated by the fact that 256-bit intermediate
    // multiplication result allows implementing population count efficiently
    // for numbers that are up to 42 bits, and 40 is the highest multiple of
    // eight below 42.
    //42位的数 40倍是最大的倍数 最大掩码数 ?????
    uint constant MAX_MASK_MODULO = 40;

    // 掩码检查 防止溢出????
    uint constant MAX_BET_MASK = 2 ** MAX_MASK_MODULO;//80

    //EVM查询过去的区块信息 256个块信息
    uint constant BET_EXPIRATION_BLOCKS = 250;

    //秘密签名人的地址
    address constant DUMMY_ADDRESS = 0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE;

    // 合同的所有权发生变更
    address public owner;
    address private nextOwner;

    //调整赌注赔率 限制最大押注的利润。
    uint public maxProfit;//最大的收益

    // 负责签名comit
    address public secretSigner;//秘密签名者 seed

    // 累计奖池
    uint128 public jackpotSize;//jackpotsize

    // 锁定 防止合同提交了不能支付的起的赌注
    uint128 public lockedInBets;//锁定的赌注

    // 单个赌注的构造体
    struct Bet {
        // 堵住数额（wei为单位）
        uint amount;
        // 区分不同游戏.
        uint8 modulo;//模
        //压住的数以下
        uint8 rollUnder;
        // 放置赌注的区块号码
        uint40 placeBlockNumber;//区块编号
        // 一个40位的掩码（不同的倍数代表不同的位掩码） (see MAX_MASK_MODULO comment).
        uint40 mask;

        // 投机者押注的地址.
        address gambler;//投注人地址
    }

    // 记录处在活跃或者进行中的赌注.
    mapping (uint => Bet) bets;

    // 赌场管理人地址
    address public croupier;

    // 统计失败支付 奖池支付
    event FailedPayment(address indexed beneficiary, uint amount);
    event Payment(address indexed beneficiary, uint amount);
    event JackpotPayment(address indexed beneficiary, uint amount);

    // 记录提交的placebet
    event Commit(uint commit);

    // 构造体。 不要传参数
    constructor () public {
        owner = msg.sender;
        secretSigner = DUMMY_ADDRESS;//虚假的地址
        croupier = DUMMY_ADDRESS;
    }

    // 标准修改器 主人调用
    modifier onlyOwner {
        require (msg.sender == owner, "OnlyOwner methods called by non-owner.");
        _;
    }

    modifier onlyCroupier {
        require (msg.sender == croupier, "OnlyCroupier methods called by non-croupier.");
        _;
    }
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //I合约管理

    // Standard contract ownership transfer implementation,
    function approveNextOwner(address _nextOwner) external onlyOwner {
        require (_nextOwner != owner, "Cannot approve current owner.");
        nextOwner = _nextOwner;
    }

    function acceptNextOwner() external {
        require (msg.sender == nextOwner, "Can only accept preapproved new owner.");
        owner = nextOwner;
    }

    // 回退函数故意保存为空
    function () public payable {
    }

    // 设置秘密签名人
    function setSecretSigner(address newSecretSigner) external onlyOwner {
        secretSigner = newSecretSigner;
    }

    // 设置管理员地址
    function setCroupier(address newCroupier) external onlyOwner {
        croupier = newCroupier;
    }

    // 可以通过设置为0使投注失效.
    function setMaxProfit(uint _maxProfit) public onlyOwner {
        require (_maxProfit < MAX_AMOUNT, "maxProfit should be a sane number.");
        maxProfit = _maxProfit;
    }

    //奖池金额只能提高不能降低
    function increaseJackpot(uint increaseAmount) external onlyOwner {
        require (increaseAmount <= address(this).balance, "Increase amount larger than balance.");
        require (jackpotSize + lockedInBets + increaseAmount <= address(this).balance, "Not enough funds.");
        jackpotSize += uint128(increaseAmount);
    }

    // 提现.
    function withdrawFunds(address beneficiary, uint withdrawAmount) external onlyOwner {
        require (withdrawAmount <= address(this).balance, "Increase amount larger than balance.");
        require (jackpotSize + lockedInBets + withdrawAmount <= address(this).balance, "Not enough funds.");
        sendFunds(beneficiary, withdrawAmount, withdrawAmount);
    }

    // 摧毁合约 所有的钱转移给合约所有者.
    function kill() external onlyOwner {
        require (lockedInBets == 0, "All bets should be processed (settled or refunded) before self-destruct.");
        selfdestruct(owner);
    }



    //////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// *** Betting logic 防止升级或者其他造成障碍
    // Bet states:
    //  amount == 0 && gambler == 0 - 'clean' (can place a bet)
    //  amount != 0 && gambler != 0 - 'active' (can be settled or refunded)
    //  amount == 0 && gambler != 0 - 'processed' (can clean storage)

    // Bet placing transaction - issued by the player.
    //  betMask         - bet outcomes bit mask for modulo <= MAX_MASK_MODULO,
    //                    [0, betMask) .赌注
    //  modulo          - 游戏模（区分游戏） 最大值 或者和.
    //  commitLastBlock - 有效区块
    //  commit          - 处理赌注时候 赌场机器人提供的一些秘密的随机数的哈希  确保不会被矿工改变。 机器人提供。

    // v r, s            - 椭圆曲线加密交易tx后的值 v=27


    //II下注

    function placeBet(uint betMask, uint modulo, uint commitLastBlock, uint commit, bytes32 r, bytes32 s) external payable {
        // 赌注处于结束状态
        Bet storage bet = bets[commit];//数量 取出接收
        require (bet.gambler == address(0), "Bet should be in a 'clean' state.");

        // 校验范围.
        uint amount = msg.value;
        require (modulo > 1 && modulo <= MAX_MODULO, "Modulo should be within range.");
        require (amount >= MIN_BET && amount <= MAX_AMOUNT, "Amount should be within range.");
        require (betMask > 0 && betMask < MAX_BET_MASK, "Mask should be within range.");

        // 赌注的签名是有效的.
        require (block.number <= commitLastBlock, "Commit has expired.");
//        abi.encodePacked(...) returns (bytes)：计算参数的紧密打包编码
        //32位的签名哈希 由commit和block生成
        bytes32 signatureHash = keccak256(abi.encodePacked(uint40(commitLastBlock), commit));//签名的哈希
        //校验签名的合法性 无效的时候给提醒 椭圆曲线无效
        require (secretSigner == ecrecover(signatureHash, 27, r, s), "ECDSA signature is not valid.");

        uint rollUnder;
        uint mask;

        //最大掩码的模 一般最大值100
        if (modulo <= MAX_MASK_MODULO) {
            rollUnder = ((betMask * POPCNT_MULT) & POPCNT_MASK) % POPCNT_MODULO;
            mask = betMask;
        } else {
            // 指定边缘 一般走第二个
            //rollUnder
            require (betMask > 0 && betMask <= modulo, "High modulo range, betMask larger than modulo.");
            rollUnder = betMask;
        }

        // 获胜赌注.
        uint possibleWinAmount;
        uint jackpotFee;//累计投注

        (possibleWinAmount, jackpotFee) = getDiceWinAmount(amount, modulo, rollUnder);

        // 获胜的金额有效
        require (possibleWinAmount <= amount + maxProfit, "maxProfit limit violation.");
        //锁定赌注 累计奖池
        lockedInBets += uint128(possibleWinAmount);//可能投注
        jackpotSize += uint128(jackpotFee);//累计投注数额

        // 检测账户余额.
        require (jackpotSize + lockedInBets <= address(this).balance, "Cannot afford to lose this bet.");

        // 记录和存储 .
        emit Commit(commit);
        // Store bet parameters on blockchain.data 存储投注信息
        bet.amount = amount;
        bet.modulo = uint8(modulo);
        bet.rollUnder = uint8(rollUnder);
        bet.placeBlockNumber = uint40(block.number);
        bet.mask = uint40(mask);
        bet.gambler = msg.sender;

    }
    //赌注 赌场管理人提交的散列值 庄家调用 用来结算交易
    function settleBet(uint reveal, bytes32 blockHash) external onlyCroupier {
        uint commit = uint(keccak256(abi.encodePacked(reveal)));

        Bet storage bet = bets[commit];
        uint placeBlockNumber = bet.placeBlockNumber;

        // 赌注是否过期
        require (block.number > placeBlockNumber, "settleBet in the same block as placeBet, or before.");
        require (block.number <= placeBlockNumber + BET_EXPIRATION_BLOCKS, "Blockhash can't be queried by EVM.");
        require (blockhash(placeBlockNumber) == blockHash);

        // 结算赌注
        settleBetCommon(bet, reveal, blockHash);
    }
    //数块
    // 解决Merkle(默克尔书)树产生问题 树快产生  一个区块存储了两个哈希
    function settleBetUncleMerkleProof(uint reveal, uint40 canonicalBlockNumber) external onlyCroupier {
        // "commit" 必须包含 "reveal".
        uint commit = uint(keccak256(abi.encodePacked(reveal)));

        Bet storage bet = bets[commit];

        require (block.number <= canonicalBlockNumber + BET_EXPIRATION_BLOCKS, "Blockhash can't be queried by EVM.");


        requireCorrectReceipt(4 + 32 + 32 + 4);

        // 来自merkle树的哈希证明 , 验证他们.
        bytes32 canonicalHash;
        bytes32 uncleHash;
        (canonicalHash, uncleHash) = verifyMerkleProof(commit, 4 + 32 + 32);
        require (blockhash(canonicalBlockNumber) == canonicalHash);

        // 使用熵值和树快结算赌注.
        settleBetCommon(bet, reveal, uncleHash);
    }

    // 用于公共赌注结算 区块的哈希 赌注 揭露数（网站提供）
    function settleBetCommon(Bet storage bet, uint reveal, bytes32 entropyBlockHash) private {
        uint amount = bet.amount;
        uint modulo = bet.modulo;
        uint rollUnder = bet.rollUnder;
        address gambler = bet.gambler;


        require (amount != 0, "Bet should be in an 'active' state");

     //赌注已经进行过
        bet.amount = 0;

        // 防止矿工。
        //随机数生成干扰 打包编码 取哈希值并取模
        bytes32 entropy = keccak256(abi.encodePacked(reveal, entropyBlockHash));

        //取模计算 熵 关键
        uint dice = uint(entropy) % modulo;

        uint diceWinAmount;
        uint _jackpotFee;
        (diceWinAmount, _jackpotFee) = getDiceWinAmount(amount, modulo, rollUnder);

        uint diceWin = 0;
        uint jackpotWin = 0;

        if (modulo <= MAX_MASK_MODULO) {
            // 小规模游戏 位掩码
            if ((2 ** dice) & bet.mask != 0) {
                diceWin = diceWinAmount;
            }

        } else {
            if (dice < rollUnder) { //此种情况获胜 dice 生成的骰子 10
                diceWin = diceWinAmount;
            }
        }

        // 解锁
        lockedInBets -= uint128(diceWinAmount);

        // Roll for a jackpot (if eligible).
        if (amount >= MIN_JACKPOT_BET) {
            // 第二个摸独立存在。 两个游戏。
            //熵取模
            uint jackpotRng = (uint(entropy) / modulo) % JACKPOT_MODULO;

            // Bingo!
            if (jackpotRng == 0) {
                jackpotWin = jackpotSize;
                jackpotSize = 0;
            }
        }

        // Log jackpot win.
        if (jackpotWin > 0) {
            //提交僵持支付
            emit JackpotPayment(gambler, jackpotWin);
        }

        // 给获胜者发送. 发送1wei 或者发送奖金
        sendFunds(gambler, diceWin + jackpotWin == 0 ? 1 wei : diceWin + jackpotWin, diceWin);
    }
//////////////////////////////////////////////////////////////////////////////////////////
    //退款  参数输入了指定的commit
    function refundBet(uint commit) external {
        // Check that bet is in 'active' state.
        Bet storage bet = bets[commit];
        uint amount = bet.amount;

        require (amount != 0, "Bet should be in an 'active' state");

        // 检测是否过期
        require (block.number > bet.placeBlockNumber + BET_EXPIRATION_BLOCKS, "Blockhash can't be queried by EVM.");

        // 处理过的
        bet.amount = 0;

        uint diceWinAmount;
        uint jackpotFee;
        (diceWinAmount, jackpotFee) = getDiceWinAmount(amount, bet.modulo, bet.rollUnder);

        lockedInBets -= uint128(diceWinAmount);
        jackpotSize -= uint128(jackpotFee);


        sendFunds(bet.gambler, amount, amount);
    }

    // 减去庄家赢的钱数 剩下的就是你赢的钱  要求庄家必须盈利
    function getDiceWinAmount(uint amount, uint modulo, uint rollUnder) private pure returns (uint winAmount, uint jackpotFee) {
        require (0 < rollUnder && rollUnder <= modulo, "Win probability out of range.");

        jackpotFee = amount >= MIN_JACKPOT_BET ? JACKPOT_FEE : 0;
        //百分比之一
        uint houseEdge = amount * HOUSE_EDGE_PERCENT / 100;

        if (houseEdge < HOUSE_EDGE_MINIMUM_AMOUNT) {
            houseEdge = HOUSE_EDGE_MINIMUM_AMOUNT;
        }

        require (houseEdge + jackpotFee <= amount, "Bet doesn't even cover house edge.");
        //盈利的利润
      winAmount = (amount - houseEdge - jackpotFee) * modulo / rollUnder;
    }

    // 指定奖励 支付成功输出
    function sendFunds(address beneficiary, uint amount, uint successLogAmount) private {
        if (beneficiary.send(amount)) {
            emit Payment(beneficiary, successLogAmount);

        } else {
            emit FailedPayment(beneficiary, amount);
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////
    //一些技术性的工具  用于位运算
    uint constant POPCNT_MULT = 0x0000000000002000000000100000000008000000000400000000020000000001;

    uint constant POPCNT_MASK = 0x0001041041041041041041041041041041041041041041041041041041041041;

    uint constant POPCNT_MODULO = 0x3F;

    // *** Merkle证明  用于证明加密的安全性
    // Helper to verify a full merkle proof starting from some seedHash (usually commit). "offset" is the location of the proof
    // beginning in the calldata.
    function verifyMerkleProof(uint seedHash, uint offset) pure private returns (bytes32 blockHash, bytes32 uncleHash) {
        // (Safe) assumption - nobody will write into RAM during this method invocation.
        uint scratchBuf1;  assembly { scratchBuf1 := mload(0x40) }

        uint uncleHeaderLength; uint blobLength; uint shift; uint hashSlot;

        // Verify merkle proofs up to uncle block header. Calldata layout is:
        //  - 2 byte big-endian slice length
        //  - 2 byte big-endian offset to the beginning of previous slice hash within the current slice (should be zeroed)
        //  - followed by the current slice verbatim
        for (;; offset += blobLength) {
            assembly { blobLength := and(calldataload(sub(offset, 30)), 0xffff) }
            if (blobLength == 0) {
                // Zero slice length marks the end of uncle proof.
                break;
            }

            assembly { shift := and(calldataload(sub(offset, 28)), 0xffff) }
            require (shift + 32 <= blobLength, "Shift bounds check.");

            offset += 4;
            assembly { hashSlot := calldataload(add(offset, shift)) }
            require (hashSlot == 0, "Non-empty hash slot.");

            assembly {
                calldatacopy(scratchBuf1, offset, blobLength)
                mstore(add(scratchBuf1, shift), seedHash)
                seedHash := sha3(scratchBuf1, blobLength)
                uncleHeaderLength := blobLength
            }
        }

        // At this moment the uncle hash is known.
        uncleHash = bytes32(seedHash);

        // Construct the uncle list of a canonical block.
        uint scratchBuf2 = scratchBuf1 + uncleHeaderLength;
        uint unclesLength; assembly { unclesLength := and(calldataload(sub(offset, 28)), 0xffff) }
        uint unclesShift;  assembly { unclesShift := and(calldataload(sub(offset, 26)), 0xffff) }
        require (unclesShift + uncleHeaderLength <= unclesLength, "Shift bounds check.");

        offset += 6;
        assembly { calldatacopy(scratchBuf2, offset, unclesLength) }
        memcpy(scratchBuf2 + unclesShift, scratchBuf1, uncleHeaderLength);

        assembly { seedHash := sha3(scratchBuf2, unclesLength) }

        offset += unclesLength;

        // Verify the canonical block header using the computed sha3Uncles.
        assembly {
            blobLength := and(calldataload(sub(offset, 30)), 0xffff)
            shift := and(calldataload(sub(offset, 28)), 0xffff)
        }
        require (shift + 32 <= blobLength, "Shift bounds check.");

        offset += 4;
        assembly { hashSlot := calldataload(add(offset, shift)) }
        require (hashSlot == 0, "Non-empty hash slot.");

        assembly {
            calldatacopy(scratchBuf1, offset, blobLength)
            mstore(add(scratchBuf1, shift), seedHash)

        // At this moment the canonical block hash is known.
            blockHash := sha3(scratchBuf1, blobLength)
        }
    }


    // Helper to check the placeBet receipt. "offset" is the location of the proof beginning in the calldata.开始位置
    //帮助检查赌注 位移
    // RLP layout: [triePath, str([status, cumGasUsed, bloomFilter, [[address, [topics], data]])]
    function requireCorrectReceipt(uint offset) view private {
        //定义赋值
        uint leafHeaderByte;//头部字节
        assembly { leafHeaderByte := byte(0, calldataload(offset)) }
        //判断0xf7 大于55个字节 一个字符一般两个字节 55位长度
        require (leafHeaderByte >= 0xf7, "Receipt leaf longer than 55 bytes.");
        offset += leafHeaderByte - 0xf6;
        //内码高字节范围0xa1--0xf7 内低字节码范围0xa1--0xfe 小路头部字节


        uint pathHeaderByte;
        assembly { pathHeaderByte := byte(0, calldataload(offset)) }//从指定位置调用函数
        if (pathHeaderByte <= 0x7f) {
            offset += 1;

        } else {
            //二进制字符串
            require (pathHeaderByte >= 0x80 && pathHeaderByte <= 0xb7, "Path is an RLP string.");
            offset += pathHeaderByte - 0x7f;
        }


            //收到的字符串总是字符串 并且小于64k 否则不接受
        uint receiptStringHeaderByte;
        assembly { receiptStringHeaderByte := byte(0, calldataload(offset)) }
        require (receiptStringHeaderByte == 0xb9, "Receipt string is always at least 256 bytes long, but less than 64k.");
        offset += 3;

            //同样的头部字节也是类似
        uint receiptHeaderByte; assembly { receiptHeaderByte := byte(0, calldataload(offset)) }
        require (receiptHeaderByte == 0xf9, "Receipt is always at least 256 bytes long, but less than 64k.");
        offset += 3;
        //状态字节是0x1 16机制编码 第一个数字是1
        uint statusByte; assembly { statusByte := byte(0, calldataload(offset)) }
        require (statusByte == 0x1, "Status should be success.");
        offset += 1;

            //附加gas头部字节
        uint cumGasHeaderByte; assembly { cumGasHeaderByte := byte(0, calldataload(offset)) }
        if (cumGasHeaderByte <= 0x7f) {
            offset += 1;

        } else {
            //累加的gas是一个rlp字符
            require (cumGasHeaderByte >= 0x80 && cumGasHeaderByte <= 0xb7, "Cumulative gas is an RLP string.");
            offset += cumGasHeaderByte - 0x7f;
        }
        //开花头部
        uint bloomHeaderByte; assembly { bloomHeaderByte := byte(0, calldataload(offset)) }
        require (bloomHeaderByte == 0xb9, "Bloom filter is always 256 bytes long.");
        offset += 256 + 3;
        //log的列表头部字节 256字节
        uint logsListHeaderByte; assembly { logsListHeaderByte := byte(0, calldataload(offset)) }
        require (logsListHeaderByte == 0xf8, "Logs list is less than 256 bytes long.");
        offset += 2;
        //log256个字节
        uint logEntryHeaderByte; assembly { logEntryHeaderByte := byte(0, calldataload(offset)) }
        require (logEntryHeaderByte == 0xf8, "Log entry is less than 256 bytes long.");
        offset += 2;
        //地址是20比特长
        uint addressHeaderByte; assembly { addressHeaderByte := byte(0, calldataload(offset)) }
        require (addressHeaderByte == 0x94, "Address is 20 bytes long.");
        //Log地址等于唯一地址  求恶化 区域加上11 等同于storage
        uint logAddress; assembly { logAddress := and(calldataload(sub(offset, 11)), 0xffffffffffffffffffffffffffffffffffffffff) }
        require (logAddress == uint(address(this)));
    }

    // Memory copy. 内存拷贝
    function memcpy(uint dest, uint src, uint len) pure private {
        // Full 32 byte words
        for(; len >= 32; len -= 32) {
            assembly { mstore(dest, mload(src)) }
            dest += 32; src += 32;
        }

        // Remaining bytes 余下的字节 乘方运算
        uint mask = 256 ** (32 - len) - 1;
        assembly {
            let srcpart := and(mload(src), not(mask))
            let destpart := and(mload(dest), mask)
            mstore(dest, or(destpart, srcpart))
        }
    }
}