-- DLXSimulator.sml (MLton benchmarks, 3cps test/programs/from-mlton;
-- Matthew Fluet): a simulator for the DLX instruction set with a register
-- file, a 65536-word memory and a 2-way set-associative cache, all as
-- immutable arrays stored as lists (O(n) sub and update), running five
-- small DLX programs (the last computes a GCD; of 1234 and 987 here, not 123456789 and 98765: the lazy translation keeps old memory versions alive, and the original input needs 7 GB).  Pure, as in the SML.  The
-- SML's only effects inside that pure code are the cache's global random
-- number generator and error messages on paths these programs never take;
-- both use unsafePerformIO in NOINLINE functions.  MLton's Int is 32 bits:
-- the ALU's signed ADD and SUB check for overflow as the SML's do.  Prints
-- what the SML prints on its last round.
module Main (main) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int32)
import Data.IORef
import Data.Word (Word32)
import Numeric (readHex)
import System.IO.Unsafe (unsafePerformIO)
import Repeat

-- An error message from pure code (never printed by these programs)
warn :: String -> a -> a
warn msg x = unsafePerformIO (putStr msg) `seq` x
{-# NOINLINE warn #-}

smlInt :: Int -> String
smlInt n = if n < 0 then '~' : show (negate n) else show n

toIntX :: Word32 -> Int
toIntX w = fromIntegral (fromIntegral w :: Int32)

fromInt :: Int -> Word32
fromInt = fromIntegral

sra :: Word32 -> Int -> Word32
sra w n = fromIntegral ((fromIntegral w :: Int32) `shiftR` n)

-- From page 284 of Numerical Recipes in C (a global seed in the SML)
seedRef :: IORef Word32
seedRef = unsafePerformIO (newIORef 13)
{-# NOINLINE seedRef #-}

rand :: () -> Word32
rand () = unsafePerformIO $ do
  s <- readIORef seedRef
  let res = 1664525 * s + 1013904223
  writeIORef seedRef res
  return res
{-# NOINLINE rand #-}

------------------------------------------------------------------------
-- ImmArray: immutable arrays as lists

newtype IA a = IA [a] deriving Eq

iaTabulate :: Int -> (Int -> a) -> IA a
iaTabulate n f = IA (map f [0 .. n - 1])

immarray :: Int -> a -> IA a
immarray n x = iaTabulate n (const x)

iaSub :: IA a -> Int -> a
iaSub (IA ia) i = ia !! i

iaUpdate :: IA a -> Int -> a -> IA a
iaUpdate (IA ia) i x = IA (take i ia ++ (x : drop (i + 1) ia))

-- foldr f b (IA ia) = foldri (fn (_,i,x) => f(i,x)) b (IA ia, 0, NONE)
iaFoldr :: (a -> b -> b) -> b -> IA a -> b
iaFoldr f b (IA ia) = foldriAux 0 ia
  where foldriAux _ [] = b
        foldriAux i (h : t) = f h (foldriAux (i + 1 :: Int) t)

iaMap :: (a -> b) -> IA a -> IA b
iaMap f (IA ia) = IA (mapiAux 0 ia)
  where mapiAux _ [] = []
        mapiAux i (h : t) = f h : mapiAux (i + 1 :: Int) t

------------------------------------------------------------------------
-- RegisterFile

type RegisterFile = IA Word32

initRegisterFile :: RegisterFile
initRegisterFile =
  iaUpdate (iaUpdate (iaUpdate (iaUpdate (immarray 32 0) 0 0) 28 0) 29 0x00040000) 30 0x00040000

loadRegister :: RegisterFile -> Int -> Word32
loadRegister = iaSub

storeRegister :: RegisterFile -> Int -> Word32 -> RegisterFile
storeRegister = iaUpdate

------------------------------------------------------------------------
-- ALU

data ALUOp = ASLL | ASRL | ASRA | AADD | AADDU | ASUB | ASUBU | AAND | AOR | AXOR
           | ASEQ | ASNE | ASLT | ASGT | ASLE | ASGE

-- Signed 32-bit arithmetic; Nothing on overflow (the SML's Overflow)
int32 :: Int -> Maybe Int
int32 r = if r < -2147483648 || r > 2147483647 then Nothing else Just r

performAL :: ALUOp -> Word32 -> Word32 -> Word32
performAL opcode s1 s2 = case opcode of
  ASLL -> s1 `shiftL` fromIntegral s2
  ASRL -> s1 `shiftR` fromIntegral s2
  ASRA -> sra s1 (fromIntegral s2)
  AADD -> checked (toIntX s1 + toIntX s2)
  AADDU -> s1 + s2
  ASUB -> checked (toIntX s1 - toIntX s2)
  ASUBU -> s1 - s2
  AAND -> s1 .&. s2
  AOR -> s1 .|. s2
  AXOR -> s1 `xor` s2
  ASEQ -> if s1 == s2 then 1 else 0
  ASNE -> if not (s1 == s2) then 1 else 0
  ASLT -> if toIntX s1 < toIntX s2 then 1 else 0
  ASGT -> if toIntX s1 > toIntX s2 then 1 else 0
  ASLE -> if toIntX s1 <= toIntX s2 then 1 else 0
  ASGE -> if toIntX s1 >= toIntX s2 then 1 else 0
  where checked r = case int32 r of
          Just v -> fromInt v
          Nothing -> warn "Error : ALU returning 0\n" 0

------------------------------------------------------------------------
-- Memory

type Memory = (IA Word32, (Int, Int))

initMemory :: Memory
initMemory = (immarray 0x10000 0, (0, 0))

alignWAddress, alignHWAddress :: Word32 -> Word32
alignWAddress address = (address `shiftR` 2) `shiftL` 2
alignHWAddress address = (address `shiftR` 1) `shiftL` 1

mLoad :: Memory -> Word32 -> (Memory, Word32)
mLoad (mem, (reads, writes)) address =
  let alignedAddress = alignWAddress address
      useAddress = alignedAddress `shiftR` 2
  in ((mem, (reads + 1, writes)), iaSub mem (fromIntegral useAddress))

mStore :: Memory -> Word32 -> Word32 -> Memory
mStore (mem, (reads, writes)) address data' =
  let alignedAddress = alignWAddress address
      useAddress = alignedAddress `shiftR` 2
  in (iaUpdate mem (fromIntegral useAddress) data', (reads, writes + 1))

mLoadWord :: Memory -> Word32 -> (Memory, Word32)
mLoadWord mem address =
  let alignedAddress = if address == alignWAddress address then address
                       else warn "Error LW: Memory using aligned address\n" (alignWAddress address)
  in mLoad mem alignedAddress

mStoreWord :: Memory -> Word32 -> Word32 -> Memory
mStoreWord mem address data' =
  let alignedAddress = if address == alignWAddress address then address
                       else warn "Error SW: Memory using aligned address\n" (alignWAddress address)
  in mStore mem alignedAddress data'

mGetStatistics :: Memory -> String
mGetStatistics (_, (reads, writes)) =
  "Memory :\n" ++ "Memory Reads : " ++ smlInt reads ++ "\n" ++ "Memory Writes : " ++ smlInt writes ++ "\n"

------------------------------------------------------------------------
-- CachedMemory (L1CacheSpec1 over Memory)

type CacheLine = (Bool, Bool, Word32, IA Word32)
type CacheSet = IA CacheLine
type Cache = IA CacheSet
type CMemory = ((Cache, (Int, Int, Int, Int)), Memory)

data WriteHitOption = WriteThrough | WriteBack
data WriteMissOption = WriteAllocate | WriteNoAllocate

cacheName :: String
cacheName = "Level 1 Cache"

cacheSize, blockSize, associativity :: Int
cacheSize = 256
blockSize = 4
associativity = 2

writeHit :: WriteHitOption
writeHit = WriteThrough

writeMiss :: WriteMissOption
writeMiss = WriteNoAllocate

exp2 :: Int -> Int
exp2 0 = 1
exp2 n = 2 * exp2 (n - 1)

log2 :: Int -> Int
log2 x = log2Aux 0
  where log2Aux n = if exp2 n > x then n - 1 else log2Aux (n + 1)

indexSize, blockOffsetBits, indexBits, tagBits :: Int
indexSize = cacheSize `div` (blockSize * associativity)
blockOffsetBits = log2 (blockSize * 4)
indexBits = log2 indexSize
tagBits = 32 - blockOffsetBits - indexBits

-- A random entry of a set (the address only keeps the call in place)
randEntry :: Word32 -> Int
randEntry _ = let modulus = fromIntegral (associativity - 1) :: Word32
              in fromIntegral (rand () `mod` modulus)
{-# NOINLINE randEntry #-}

initCache :: (Cache, (Int, Int, Int, Int))
initCache =
  let cacheline = (False, False, 0, immarray blockSize 0)
      cacheset = immarray associativity cacheline
  in (immarray indexSize cacheset, (0, 0, 0, 0))

cInitMemory :: CMemory
cInitMemory = (initCache, initMemory)

getTag :: Word32 -> Word32
getTag address = address `shiftR` (indexBits + blockOffsetBits)

getIndex :: Word32 -> Word32
getIndex address =
  let mask = complement ((0xFFFFFFFF `shiftR` (indexBits + blockOffsetBits)) `shiftL` (indexBits + blockOffsetBits))
  in (address .&. mask) `shiftR` blockOffsetBits

getBlockOffset :: Word32 -> Word32
getBlockOffset address =
  let mask = complement ((0xFFFFFFFF `shiftR` blockOffsetBits) `shiftL` blockOffsetBits)
  in address .&. mask

inCacheAuxEntry :: CacheLine -> Word32 -> Bool
inCacheAuxEntry (valid, _, tag, _) address = tag == getTag address && valid

inCacheAuxSet :: CacheSet -> Word32 -> Bool
inCacheAuxSet set address = iaFoldr (\entry result -> inCacheAuxEntry entry address || result) False set

inCache :: Cache -> Word32 -> Bool
inCache cac address = inCacheAuxSet (iaSub cac (fromIntegral (getIndex address))) address

readCacheAuxEntry :: CacheLine -> Word32 -> Word32
readCacheAuxEntry (_, _, _, block) address = iaSub block (fromIntegral (getBlockOffset address `shiftR` 2))

readCacheAuxSet :: CacheSet -> Word32 -> Word32
readCacheAuxSet set address =
  iaFoldr (\entry result -> if inCacheAuxEntry entry address then readCacheAuxEntry entry address else result) 0 set

readCache :: Cache -> Word32 -> Word32
readCache cac address = readCacheAuxSet (iaSub cac (fromIntegral (getIndex address))) address

writeCacheAuxEntry :: CacheLine -> Word32 -> Word32 -> CacheLine
writeCacheAuxEntry (_, _, tag, block) address data' =
  let ndirty = case writeHit of { WriteThrough -> False; WriteBack -> True }
  in (True, ndirty, tag, iaUpdate block (fromIntegral (getBlockOffset address `shiftR` 2)) data')

writeCacheAuxSet :: CacheSet -> Word32 -> Word32 -> CacheSet
writeCacheAuxSet set address data' =
  iaMap (\entry -> if inCacheAuxEntry entry address then writeCacheAuxEntry entry address data' else entry) set

writeCache :: Cache -> Word32 -> Word32 -> Cache
writeCache cac address data' =
  let index = fromIntegral (getIndex address)
      nset = writeCacheAuxSet (iaSub cac index) address data'
  in iaUpdate cac index nset

loadBlock :: Memory -> Word32 -> (IA Word32, Memory)
loadBlock mem address =
  iaFoldr (\offset (block, mem') ->
             let laddress = ((address `shiftR` blockOffsetBits) `shiftL` blockOffsetBits) + (fromInt offset `shiftL` 2)
                 (nmem, nword) = mLoadWord mem' laddress
             in (iaUpdate block offset nword, nmem))
          (immarray blockSize 0, mem)
          (iaTabulate blockSize id)

storeBlock :: IA Word32 -> Memory -> Word32 -> Memory
storeBlock block mem address =
  iaFoldr (\offset mem' ->
             let saddress = ((address `shiftR` blockOffsetBits) `shiftL` blockOffsetBits) + (fromInt offset `shiftL` 2)
             in mStoreWord mem' saddress (iaSub block offset))
          mem
          (iaTabulate blockSize id)

loadCacheAuxEntry :: CacheLine -> Memory -> Word32 -> (CacheLine, Memory)
loadCacheAuxEntry (valid, dirty, tag, block) mem address =
  let saddress = (tag `shiftL` tagBits) .|. (getIndex address `shiftL` indexBits)
      nmem = if valid && dirty then storeBlock block mem saddress else mem
      (nblock, nnmem) = loadBlock nmem address
  in ((True, False, getTag address, nblock), nnmem)

loadCacheAuxSet :: CacheSet -> Memory -> Word32 -> (CacheSet, Memory)
loadCacheAuxSet set mem address =
  let entry = randEntry address
      (nentry, nmem) = loadCacheAuxEntry (iaSub set entry) mem address
  in (iaUpdate set entry nentry, nmem)

loadCache :: Cache -> Memory -> Word32 -> (Cache, Memory)
loadCache cac mem address =
  let index = fromIntegral (getIndex address)
      (nset, nmem) = loadCacheAuxSet (iaSub cac index) mem address
  in (iaUpdate cac index nset, nmem)

cLoad :: CMemory -> Word32 -> (CMemory, Word32)
cLoad ((cac, (rh, rm, wh, wm)), mem) address =
  let alignedAddress = alignWAddress address
  in if inCache cac alignedAddress
       then (((cac, (rh + 1, rm, wh, wm)), mem), readCache cac alignedAddress)
       else let (ncac, nmem) = loadCache cac mem alignedAddress
            in (((ncac, (rh, rm + 1, wh, wm)), nmem), readCache ncac alignedAddress)

cStore :: CMemory -> Word32 -> Word32 -> CMemory
cStore ((cac, (rh, rm, wh, wm)), mem) address data' =
  let alignedAddress = alignWAddress address
  in if inCache cac alignedAddress
       then let ncac = writeCache cac alignedAddress data'
            in case writeHit of
                 WriteThrough -> ((ncac, (rh, rm, wh + 1, wm)), mStoreWord mem alignedAddress data')
                 WriteBack -> ((ncac, (rh, rm, wh + 1, wm)), mem)
       else case writeMiss of
              WriteAllocate ->
                let (ncac, nmem) = loadCache cac mem alignedAddress
                    nncac = writeCache ncac alignedAddress data'
                in case writeHit of
                     WriteThrough -> ((nncac, (rh, rm, wh, wm + 1)), mStoreWord nmem alignedAddress data')
                     WriteBack -> ((nncac, (rh, rm, wh, wm + 1)), nmem)
              WriteNoAllocate -> ((cac, (rh, rm, wh, wm + 1)), mStoreWord mem alignedAddress data')

cLoadWord :: CMemory -> Word32 -> (CMemory, Word32)
cLoadWord mem address =
  let alignedAddress = if address == alignWAddress address then address
                       else warn "Error LW: Memory using aligned address\n" (alignWAddress address)
  in cLoad mem alignedAddress

cStoreWord :: CMemory -> Word32 -> Word32 -> CMemory
cStoreWord mem address data' =
  let alignedAddress = if address == alignWAddress address then address
                       else warn "Error SW: Memory using aligned address\n" (alignWAddress address)
  in cStore mem alignedAddress data'

cLoadHWord, cLoadHWordU, cLoadByte, cLoadByteU :: CMemory -> Word32 -> (CMemory, Word32)
cLoadHWord mem address =
  let alignedAddress = if address == alignHWAddress address then address
                       else warn "Error LH: Memory using aligned address\n" (alignHWAddress address)
      (nmem, lWord) = cLoad mem alignedAddress
  in (nmem, case alignedAddress of
              0x00000000 -> sra (lWord `shiftL` 16) 16
              0x00000010 -> sra (lWord `shiftL` 0) 16
              _ -> warn "Error LH: Memory returning 0\n" 0)
cLoadHWordU mem address =
  let alignedAddress = if address == alignHWAddress address then address
                       else warn "Error LHU: Memory using aligned address\n" (alignHWAddress address)
      (nmem, lWord) = cLoad mem alignedAddress
  in (nmem, case alignedAddress of
              0x00000000 -> (lWord `shiftL` 16) `shiftR` 16
              0x00000010 -> (lWord `shiftL` 0) `shiftR` 16
              _ -> warn "Error LHU: Memory returning 0\n" 0)
cLoadByte mem address =
  let (nmem, lWord) = cLoad mem address
  in (nmem, case address of
              0x00000000 -> sra (lWord `shiftL` 24) 24
              0x00000008 -> sra (lWord `shiftL` 16) 24
              0x00000010 -> sra (lWord `shiftL` 8) 24
              0x00000018 -> sra (lWord `shiftL` 0) 24
              _ -> warn "Error LB: Memory returning 0\n" 0)
cLoadByteU mem address =
  let (nmem, lWord) = cLoad mem address
  in (nmem, case address of
              0x00000000 -> (lWord `shiftL` 24) `shiftR` 24
              0x00000008 -> (lWord `shiftL` 16) `shiftR` 24
              0x00000010 -> (lWord `shiftL` 8) `shiftR` 24
              0x00000018 -> (lWord `shiftL` 0) `shiftR` 24
              _ -> warn "Error LBU: Memory returning 0\n" 0)

cStoreByte :: CMemory -> Word32 -> Word32 -> CMemory
cStoreByte mem address data' =
  let (_, sWord) = cLoad mem address
      put m s = cStore mem address ((m .&. sWord) .|. ((0x000000FF .&. data') `shiftL` s))
  in case address of
       0x00000000 -> put 0xFFFFFF00 0
       0x00000008 -> put 0xFFFF00FF 8
       0x00000010 -> put 0xFF00FFFF 16
       0x00000018 -> put 0x00FFFFFF 24
       _ -> warn "Error SB: Memory unchanged\n" mem

cGetStatistics :: CMemory -> String
cGetStatistics ((_, (rh, rm, wh, wm)), mem) =
  let th = rh + wh
      tm = rm + wm
      who = case writeHit of { WriteThrough -> "Write Through"; WriteBack -> "Write Back" }
      wmo = case writeMiss of { WriteAllocate -> "Write Allocate"; WriteNoAllocate -> "Write No Allocate" }
  in cacheName ++ " :\n" ++
     "CacheSize : " ++ smlInt cacheSize ++ "\n" ++
     "BlockSize : " ++ smlInt blockSize ++ "\n" ++
     "Associativity : " ++ smlInt associativity ++ "\n" ++
     "Write Hit : " ++ who ++ "\n" ++
     "Write Miss : " ++ wmo ++ "\n" ++
     "Read hits : " ++ smlInt rh ++ "\n" ++
     "Read misses : " ++ smlInt rm ++ "\n" ++
     "Write hits : " ++ smlInt wh ++ "\n" ++
     "Write misses : " ++ smlInt wm ++ "\n" ++
     "Total hits : " ++ smlInt th ++ "\n" ++
     "Total misses : " ++ smlInt tm ++ "\n" ++
     mGetStatistics mem

------------------------------------------------------------------------
-- DLXSimulator (over the cached memory)

data Opcode = SPECIAL | BEQZ | BNEZ | ADDI | ADDUI | SUBI | SUBUI | ANDI | ORI | XORI | LHI
            | SLLI | SRLI | SRAI | SEQI | SNEI | SLTI | SGTI | SLEI | SGEI
            | LB | LBU | SB | LH | LHU | SH | LW | SW | J | JAL | TRAP | JR | JALR | NON_OP
  deriving Eq

data RRFunctCode = NOP | SLL | SRL | SRA | ADD | ADDU | SUB | SUBU | AND | OR | XOR
                 | SEQ | SNE | SLT | SGT | SLE | SGE | NON_FUNCT
  deriving Eq

data Instruction = ITYPE Opcode Int Int Word32
                 | RTYPE Opcode Int Int Int Int RRFunctCode
                 | JTYPE Opcode Word32
                 | ILLEGAL
  deriving Eq

data Trap s = Trap { inputFn :: s -> (Int, s), outputFn :: Int -> s -> s, state :: s }

type Machine s = (Word32, RegisterFile, CMemory, Trap s)

halt :: Instruction
halt = JTYPE TRAP 0

decodeIType :: Word32 -> Instruction
decodeIType instr =
  let opc = (instr `shiftR` 0x1A) .&. 0x3F
      opcode = case opc of
        0x04 -> BEQZ; 0x05 -> BNEZ; 0x08 -> ADDI; 0x09 -> ADDUI; 0x0A -> SUBI; 0x0B -> SUBUI
        0x0C -> ANDI; 0x0D -> ORI; 0x0E -> XORI; 0x0F -> LHI; 0x14 -> SLLI; 0x16 -> SRLI
        0x17 -> SRAI; 0x18 -> SEQI; 0x19 -> SNEI; 0x1A -> SLTI; 0x1B -> SGTI; 0x1C -> SLEI
        0x1D -> SGEI; 0x20 -> LB; 0x24 -> LBU; 0x28 -> SB; 0x21 -> LH; 0x25 -> LHU
        0x29 -> SH; 0x23 -> LW; 0x2B -> SW
        _ -> warn "Error : Non I-Type opcode\n" NON_OP
      rs1 = fromIntegral ((instr `shiftR` 0x15) .&. 0x1F)
      rd = fromIntegral ((instr `shiftR` 0x10) .&. 0x1F)
      immediate = sra (instr `shiftL` 0x10) 0x10
  in if opcode == NON_OP then ILLEGAL else ITYPE opcode rs1 rd immediate

decodeRType :: Word32 -> Instruction
decodeRType instr =
  let rs1 = fromIntegral ((instr `shiftR` 0x15) .&. 0x1F)
      rs2 = fromIntegral ((instr `shiftR` 0x10) .&. 0x1F)
      rd = fromIntegral ((instr `shiftR` 0x0B) .&. 0x1F)
      shamt = fromIntegral ((instr `shiftR` 0x06) .&. 0x1F)
      funct = instr .&. 0x3F
      functcode = case funct of
        0x00 -> NOP; 0x04 -> SLL; 0x06 -> SRL; 0x07 -> SRA; 0x20 -> ADD; 0x21 -> ADDU
        0x22 -> SUB; 0x23 -> SUBU; 0x24 -> AND; 0x25 -> OR; 0x26 -> XOR; 0x28 -> SEQ
        0x29 -> SNE; 0x2A -> SLT; 0x2B -> SGT; 0x2C -> SLE; 0x2D -> SGE
        _ -> warn "Error : Non R-type funct\n" NON_FUNCT
  in if functcode == NON_FUNCT then ILLEGAL else RTYPE SPECIAL rs1 rs2 rd shamt functcode

decodeJType :: Word32 -> Instruction
decodeJType instr =
  let opc = (instr `shiftR` 0x1A) .&. 0x3F
      opcode = case opc of
        0x02 -> J; 0x03 -> JAL; 0x11 -> TRAP; 0x12 -> JR; 0x13 -> JALR
        _ -> warn "Error : Non J-type opcode\n" NON_OP
      offset = sra (instr `shiftL` 0x06) 0x06
  in if opcode == NON_OP then ILLEGAL else JTYPE opcode offset

decodeInstr :: Word32 -> Instruction
decodeInstr instr =
  let opcode = (instr `shiftR` 0x1A) .&. 0x3F
  in case opcode of
       0x00 -> decodeRType instr
       _ | opcode `elem` [0x02, 0x03, 0x11, 0x12, 0x13] -> decodeJType instr
         | opcode `elem` [0x04, 0x05, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
                          0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
                          0x20, 0x24, 0x28, 0x21, 0x25, 0x29, 0x23, 0x2B] -> decodeIType instr
         | otherwise -> warn "Error : Unrecognized opcode\n" ILLEGAL

branch :: Word32 -> Word32 -> Word32
branch pc offset = fromInt (toIntX pc + toIntX (offset `shiftL` 2))

performIType :: Opcode -> Int -> Int -> Word32 -> Machine s -> Machine s
performIType opcode rs1 rd immediate (pc, rf, mem, trap) =
  let alu op = (pc, storeRegister rf rd (performAL op (loadRegister rf rs1) immediate), mem, trap)
      addr = loadRegister rf rs1 + immediate
      load f = let (nmem, l) = f mem addr in (pc, storeRegister rf rd l, nmem, trap)
  in case opcode of
       BEQZ -> if loadRegister rf rs1 == 0 then (branch pc immediate, rf, mem, trap) else (pc, rf, mem, trap)
       BNEZ -> if not (loadRegister rf rs1 == 0) then (branch pc immediate, rf, mem, trap) else (pc, rf, mem, trap)
       ADDI -> alu AADD
       ADDUI -> alu AADDU
       SUBI -> alu ASUB
       SUBUI -> alu ASUBU
       ANDI -> alu AAND
       ORI -> alu AOR
       XORI -> alu AXOR
       LHI -> (pc, storeRegister rf rd (immediate `shiftL` 0x10), mem, trap)
       SLLI -> (pc, storeRegister rf rd (loadRegister rf rs1 `shiftL` fromIntegral immediate), mem, trap)
       SRLI -> (pc, storeRegister rf rd (loadRegister rf rs1 `shiftR` fromIntegral immediate), mem, trap)
       SRAI -> (pc, storeRegister rf rd (sra (loadRegister rf rs1) (fromIntegral immediate)), mem, trap)
       SEQI -> alu ASEQ
       SNEI -> alu ASNE
       SLTI -> alu ASLT
       SGTI -> alu ASGT
       SLEI -> alu ASLE
       SGEI -> alu ASGE
       LB -> load cLoadByte
       LBU -> load cLoadByteU
       SB -> (pc, rf, cStoreByte mem addr (0xFF .&. loadRegister rf rd), trap)
       LH -> load cLoadHWord
       LHU -> load cLoadHWordU
       SH -> (pc, rf, cStoreByte mem addr (0xFFFF .&. loadRegister rf rd), trap)
       LW -> load cLoadWord
       SW -> (pc, rf, cStoreWord mem addr (loadRegister rf rd), trap)
       _ -> warn "Error : Non I-Type opcode, performing NOP\n" (pc, rf, mem, trap)

performRType :: Opcode -> Int -> Int -> Int -> Int -> RRFunctCode -> Machine s -> Machine s
performRType opc rs1 rs2 rd _ funct m@(pc, rf, mem, trap) =
  let alu op = (pc, storeRegister rf rd (performAL op (loadRegister rf rs1) (loadRegister rf rs2)), mem, trap)
  in case funct of
       NOP -> m
       _ | opc /= SPECIAL -> warn "Error : Non R-Type opcode, performing NOP\n" m
       SLL -> alu ASLL
       SRL -> alu ASRL
       SRA -> alu ASRA
       ADD -> alu AADD
       ADDU -> alu AADDU
       SUB -> alu ASUB
       SUBU -> alu ASUBU
       AND -> alu AAND
       OR -> alu AOR
       XOR -> alu AXOR
       SEQ -> alu ASEQ
       SNE -> alu ASNE
       SLT -> alu ASLT
       SGT -> alu ASGT
       SLE -> alu ASLE
       SGE -> alu ASGE
       NON_FUNCT -> warn "Error : Non R-Type opcode, performing NOP\n" m

performJType :: Opcode -> Word32 -> Machine s -> Machine s
performJType opcode offset m@(pc, rf, mem, trap) =
  let reg = fromIntegral ((offset `shiftR` 0x15) .&. 0x1F)
  in case opcode of
       J -> (branch pc offset, rf, mem, trap)
       JR -> (loadRegister rf reg, rf, mem, trap)
       JAL -> (branch pc offset, storeRegister rf 31 pc, mem, trap)
       JALR -> (loadRegister rf reg, storeRegister rf 31 pc, mem, trap)
       TRAP | offset == 3 ->
                let (input, st) = inputFn trap (state trap)
                in (pc, storeRegister rf 14 (fromInt input), mem, trap { state = st })
            | offset == 4 ->
                let output = toIntX (loadRegister rf 14)
                in (pc, rf, mem, trap { state = outputFn trap output (state trap) })
       _ -> warn "Error : Non J-Type opcode, performing NOP\n" m

performInstr :: Instruction -> Machine s -> Machine s
performInstr (ITYPE o rs1 rd imm) m = performIType o rs1 rd imm m
performInstr (RTYPE o rs1 rs2 rd shamt f) m = performRType o rs1 rs2 rd shamt f m
performInstr (JTYPE o off) m = performJType o off m
performInstr ILLEGAL m = m

cycleLoop :: Machine s -> (() -> String, s)
cycleLoop (pc, rf, mem, trap) =
  let (nmem, instrWord) = cLoadWord mem pc
      instr = decodeInstr instrWord
      nPC = pc + 4
  in if instr == halt || instr == ILLEGAL
       then (\() -> cGetStatistics nmem, state trap)
       else cycleLoop (performInstr instr (nPC, rf, nmem, trap))

loadProgAux :: [String] -> CMemory -> Word32 -> CMemory
loadProgAux [] mem _ = mem
loadProgAux (instrs : instrList) mem address =
  let instr = case readHex instrs of
        [(w, "")] -> w
        _ -> warn "Error : Invalid instruction format, returning NOP\n" 0
  in loadProgAux instrList (cStoreWord mem address instr) (address + 4)

loadProg :: [String] -> CMemory -> CMemory
loadProg instrList mem = loadProgAux instrList mem 0x00010000

runProg :: [String] -> Trap s -> (s, () -> String)
runProg instructions trap =
  let (statistics, st) = cycleLoop (0x00010000, initRegisterFile, loadProg instructions cInitMemory, trap)
  in (st, statistics)

------------------------------------------------------------------------
-- The example programs and the benchmark's driver

simple, twos, absP, fact, gcdP :: [String]
simple = ["200E002F", "44000004", "44000000", "00000000"]
twos = ["44000003", "00000000", "3D00FFFF", "3508FFFF", "010E7026", "25CE0001", "44000004",
        "00000000", "44000000", "00000000"]
absP = ["44000003", "00000000", "01C0402A", "11000002", "00000000", "000E7022", "44000004",
        "00000000", "44000000", "00000000"]
fact = ["0C000002", "00000000", "44000000", "44000003", "000E2020", "2FBD0020", "AFBF0014",
        "AFBE0010", "27BE0020", "0C000009", "00000000", "8FBE0010", "8FBF0014", "27BD0020",
        "00027020", "44000004", "00001020", "4BE00000", "00000000", "20080001", "0088402C",
        "11000004", "00000000", "20020001", "08000016", "00000000", "2FBD0004", "AFA40000",
        "28840001", "2FBD0020", "AFBF0014", "AFBE0010", "27BE0020", "0FFFFFF1", "00000000",
        "8FBE0010", "8FBF0014", "27BD0020", "8FA40000", "27BD0004", "00004020", "10800005",
        "00000000", "01024020", "28840001", "0BFFFFFB", "00000000", "01001020", "4BE00000",
        "00000000"]
gcdP = ["0C000002", "00000000", "44000000", "44000003", "00000000", "000E2020", "0080402A",
        "11000002", "00000000", "00042022", "44000003", "00000000", "000E2820", "00A0402A",
        "11000002", "00000000", "00052822", "2FBD0020", "AFBF0014", "AFBE0010", "27BE0020",
        "0C00000A", "00000000", "8FBE0010", "8FBF0014", "27BD0020", "00027020", "44000004",
        "00000000", "00001020", "4BE00000", "00000000", "14A00004", "00000000", "00801020",
        "08000013", "00000000", "0085402C", "15000006", "00000000", "00804020", "00A02020",
        "01002820", "08000002", "00000000", "00A42822", "2FBD0020", "AFBF0014", "AFBE0010",
        "27BE0020", "0FFFFFED", "00000000", "8FBE0010", "8FBF0014", "27BD0020", "4BE00000",
        "00000000"]

-- One program's run: what Main.doit prints for it on the last round
doit :: ([String], [Int]) -> String
doit (instructions, inputs) =
  let inputFn' (ins, outs) = case ins of
        [] -> (0, ([], outs))
        input : ins' -> (input, (ins', outs))
      outputFn' output (ins, outs) = (ins, output : outs)
      ((_, outputs), statistics) = runProg instructions (Trap inputFn' outputFn' (inputs, []))
  in concatMap (\output -> "Output: " ++ smlInt output ++ "\n") outputs ++ statistics () ++ "\n"

main :: IO ()
main = do
  n <- iterations 1
  let progs = [(simple, []), (twos, [10]), (absP, [-10]), (fact, [12]), (gcdP, [1234, 987])]
      outs = [ concatMap doit (vary i progs []) | i <- [1 .. n] ]
  putStr (if all (== head outs) outs then head outs else "fail\n")
