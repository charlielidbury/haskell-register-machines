# haskell-register-machines
Solves register machine questions for models of computation

My version's USP is instructions are internally stored as a function which takes one state in and returns another, there is no data type or constructors for an arbitrary internal representation. For example:
```haskell
type Register = Int
type State = [Int]
type Instruction = State -> State
type Program = [(String, Instruction)]

-- Execution
runProgram :: Program -> State -> State
runProgram = snd . head
```

## Example usage:
```haskell
*Main> executeProgram cwQ1 [0, 7]
        Encoded Program: 28203107854060481328086879018110428873467534663683041542154784104828358893193049902068413849476059888311502440908888516333284352876023307497068152870815716838046300398631559269746749994190722228057021969105149474821896376541947698731973235596771379781981747814052313100844991528654401219152480353881662185793067479057395451061106881504577449397837785645292185640664823200172861488102489631069226982131181531584967730815575314624451788107859514587353169253274941423272449220579548275198561313293830502679208728397496202024208887078782437904504305261903955591389047187542451250444006137649457288465135761010402902018730854196210652638602190393588039483854012899127408493522577701462901341621421404454601758114186427362116701483467514668749062351490471009674584265893185100865019230792719421167468086164083172322643026592210128570178798256315645590128403637838108348482043589484334473238552340500008474727220655729867413541218345764751012962388784478479527678251672321799998734003025162182196662720257572298725301397163770748218375441201961628110429462244021963394406723778681535814377725389950715240067671881201337936837442866736504322187378716053724409213912501404233944668349450133364753772800444460799534111178314372376389067645690524855534567781230829097629885498575046996050470694064428876187717440543619209734917139925068321505066531804435272661393980419682919575718291981961978512613200150023638352805206455283116067382109720751779134357800578675883261677798394433620724923553664088266980329779767291070439799253812354443665893250873934225209498183077007057826484816216881232172957063134425070693890578700682140746019204841016687780096827404901982729313325539799245659624693799302919280289888512553984503044866207120141934861515723847008558030069191798788294349571251914806814310400
        Decoded Program: 
L₀: R₁⁻ → L₂, L₁
L₁: HALT
L₂: R₁⁻ → L₃, L₄
L₃: R₁⁻ → L₅, L₄
L₄: HALT
L₅: R₀⁺ → L₀
        Initial State: [0,7]
[0,7]
[0,6]
[0,5]
[0,4]
[1,4]
[1,3]
[1,2]
[1,1]
[2,1]
[2,0]
        Final State: [2,0]
```

## Example program definitions:
```haskell
cwQ1 :: Integer
cwQ1 = encodeList [
  encodeSub 1 2 1, -- L₀: R₁⁻ → L₂, L₁
  encodeHalt,      -- L₁: HALT
  encodeSub 1 3 4, -- L₂: R₁⁻ → L₃, L₄
  encodeSub 1 5 4, -- L₃: R₁⁻ → L₅, L₄
  encodeHalt,      -- L₄: HALT
  encodeAdd 0 0]   -- L₅: R₀⁺ → L₀

cwQ2 :: Integer
cwQ2 = 2^46 * 20483
```
