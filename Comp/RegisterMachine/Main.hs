-- TODO: think about project structure, this file probably should be somewhere else
module Comp.RegisterMachine.Main where

import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Either (either)
import Data.List ((\\))
import qualified Data.List as List

import System.Console.GetOpt
import System.IO
import System.Environment (getArgs, getProgName)

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Interface
import Comp.RegisterMachine.Interpreter
import Comp.RegisterMachine.Packer

data Command = PACK | UNPACK | RUN | TRACE
  deriving (Show, Eq, Ord)

instance Read Command where
  readsPrec _ str
    | List.isPrefixOf "unpack"  str = [(UNPACK, str \\ "unpack")]
    | List.isPrefixOf "pack"    str = [(  PACK, str \\ "pack")]
    | List.isPrefixOf "run"     str = [(   RUN, str \\ "run")]
    | List.isPrefixOf "trace"   str = [( TRACE, str \\ "trace")]

data Flag = Input String | Output String | Formatting Format | R0
  deriving (Show, Read, Eq)

universalOptions :: [OptDescr Flag]
universalOptions = 
  [ Option ['o'] ["output"] (OptArg outp "FILE")  "output FILE"
  , Option ['i'] ["input"]  (OptArg inp  "FILE")  "input FILE"
  ]
 -- ++
    --[ Option ['f'] ["format"] (OptArg getFormat "FORMAT") "FORMAT for the trace of the computation"
    --]
runOptions :: [OptDescr Flag]
runOptions = universalOptions ++ 
  [ Option ['z'] ["zero-reg"] (NoArg R0) "treat first register as zero register"
--,  Option ['r'] ["register-state"] (OptArg inp "FILE") "register state input FILE"
  ]

-- TODO: -f and -r options don't work (yet)
optionsFor :: Command -> [OptDescr Flag]
optionsFor PACK = universalOptions
optionsFor UNPACK = universalOptions
optionsFor RUN = runOptions
optionsFor TRACE = runOptions

inp :: Maybe String -> Flag
inp  = Input  . fromMaybe "in.txt"

outp :: Maybe String -> Flag
outp = Output . fromMaybe "out.txt"

defaultFormat :: Format
defaultFormat = "%9($instr)[, with R = %3($r)]"

formp :: Maybe String -> Flag
formp = Formatting . fromMaybe defaultFormat

parseOptions :: String -> [String] -> [OptDescr Flag] -> IO ([Flag], [String])
parseOptions cmd argv options = 
  case getOpt RequireOrder options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++ cmd ++ " [OPTION...] files..."

getInHandle :: [Flag] -> IO Handle
getInHandle []                 = return stdin
getInHandle (Input "stdin":_)  = return stdin
getInHandle (Input filename:_) = openFile filename ReadMode
getInHandle (_:xs)             = getInHandle xs

getOutHandle :: [Flag] -> IO Handle
getOutHandle []                   = return stdout
getOutHandle (Output "stdout":_)  = return stdout
getOutHandle (Output filename:_)  = openFile filename WriteMode
getOutHandle (_:xs)               = getOutHandle xs

getFormat :: [Flag] -> Maybe Format
getFormat []                = Nothing
getFormat (Formatting f:_)  = return f
getFormat (_:xs)            = getFormat xs

isZeroSet :: [Flag] -> Bool
isZeroSet = elem R0

usageForProgName :: String -> String
usageForProgName progName =  (("usage: " ++ progName ++ " <command>\n\n") ++) . unlines $ fmap ("\t"++)
  [ "unpack - decodes register machine coded as a natural number"
  , "pack - encodes register machine as a natural number"
  , "run - runs the register machine computation with initial register values given as arguments"
  , "trace - traces computation done by a register machine with initial register values given as arguments"
  ]

main :: IO ()
main = do
  progName <- getProgName
  progArgs <- getArgs
  if length progArgs < 1
    then putStr $ usageForProgName progName
    else do let command = read (head progArgs)
            (options, rest) <- parseOptions (progName ++ " " ++ head progArgs) (tail progArgs) (optionsFor command)
            hIn <- getInHandle options
            hOut <- getOutHandle options
            input <- hGetContents hIn
            case command of
              UNPACK  -> sequence_ . fmap (hPutStrLn hOut) . prettyShowMachine . natToProg $ read input
              _       -> either (hPutStrLn stderr) 
                                (\rm -> let regPicker = if isZeroSet options then regs else args in 
                                        case command of 
                                          PACK  -> hPrint hOut $ progToNat rm
                                          RUN   -> let rs = regPicker $ fmap read rest
                                                    in hPrint hOut $ computeWithRegs rm rs
                                          TRACE -> let rs = regPicker $ fmap read rest in
                                                   let (n,lines) = traceCompWithRegs defaultFormat rm rs
                                                    in sequence_ (hPutStrLn hOut <$> lines) >> hPrint hOut n)
                                (readMachine input)
            hClose hIn
            hClose hOut

-- probably unnecessary, TODO: safe delete
printComputation :: Format -> RegisterMachine -> [Natural] -> IO Natural
printComputation format rm args = sequence_ (fmap putStrLn log) >> return n
  where (n, log) = traceComp format rm args