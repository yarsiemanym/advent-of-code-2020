namespace AdventOfCode2020.Day18

open System.Text.RegularExpressions

module MathV1 =

    type Expression =
        | Literal of Value:int64
        | Operation of Operand1:Expression * Operator:char * Operand2:Expression

        member this.Evaluate =
            match this with
            | Literal(value) -> value
            | Operation(operand1, operator, operand2) ->
                match operator with
                | '+' -> operand1.Evaluate + operand2.Evaluate
                | '*' -> operand1.Evaluate * operand2.Evaluate
                | _ -> failwithf "Invalid operator '%c'." operator
    
    let combine operand1 operator operand2 = Operation(operand1, operator, operand2)

    let rec parseExpression (text:string) = 
        if Regex.IsMatch(text, @"^\d+$") then
            Literal(int64 text)
        else
            let mutable openPren = 0
            let mutable tokenStart = -1
            let mutable tokenLength = 0
            let mutable expression = Literal(0L)
            let mutable operator = '+'

            for i in 0 .. text.Length - 1 do
                let character = text.[i]

                match character with 
                | '(' ->
                    if openPren = 0 && tokenStart < 0 then 
                        tokenStart <- i + 1
                    openPren <- openPren + 1
                | ')' -> 
                    openPren <- openPren - 1
                    if openPren = 0 && tokenStart >= 0 then 
                        tokenLength <- i - tokenStart
                | ' ' -> 
                    if openPren = 0 && tokenStart >= 0 then 
                        tokenLength <- i - tokenStart
                | '+' | '*' -> 
                    if openPren = 0 then
                        tokenStart <- i
                        tokenLength <- 1
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
                    if tokenStart < 0 then
                        tokenStart <- i
                | _ -> failwithf "Invalid character '%c' in expression '%s'." character text

                if i = text.Length - 1 && tokenStart >= 0 && tokenLength <= 0 then
                    tokenLength <- 1

                if tokenStart >= 0 && tokenLength > 0 then
                    let token = text.[tokenStart .. tokenStart + tokenLength - 1]

                    match token with
                    | "+" | "*" -> operator <- token.[0]
                    | _ -> expression <- combine expression operator (parseExpression token)

                    tokenStart <- -1
                    tokenLength <- 0

            expression
 
    let parseExpressions lines = 
        [
            for line in lines do
                parseExpression line
        ]

module MathV2 =

    type Expression =
        | Literal of Value:int64
        | Operation of Operand1:Expression * Operator:char * Operand2:Expression

        member this.Evaluate =
            match this with
            | Literal(value) -> value
            | Operation(operand1, operator, operand2) ->
                match operator with
                | '+' -> operand1.Evaluate + operand2.Evaluate
                | '*' -> operand1.Evaluate * operand2.Evaluate
                | _ -> failwithf "Invalid operator '%c'." operator

    let combine operand1 operator operand2 = 
        match operator with
        | '+' -> 
            match operand1 with
            | Literal(_) -> Operation(operand1, operator, operand2)
            | Operation(left, op, right) -> Operation(left, op, Operation(right, operator, operand2))

        | '*' -> Operation(operand1, operator, operand2)
        | _ -> failwithf "Invalid operator '%c'." operator

    let rec parseExpression (text:string) = 
        if Regex.IsMatch(text, @"^\d+$") then
            Literal(int64 text)
        else
            let mutable openPren = 0
            let mutable tokenStart = -1
            let mutable tokenLength = 0
            let mutable expression = Literal(0L)
            let mutable operator = '+'

            for i in 0 .. text.Length - 1 do
                let character = text.[i]

                match character with 
                | '(' ->
                    if openPren = 0 && tokenStart < 0 then 
                        tokenStart <- i + 1
                    openPren <- openPren + 1
                | ')' -> 
                    openPren <- openPren - 1
                    if openPren = 0 && tokenStart >= 0 then 
                        tokenLength <- i - tokenStart
                | ' ' -> 
                    if openPren = 0 && tokenStart >= 0 then 
                        tokenLength <- i - tokenStart
                | '+' | '*' -> 
                    if openPren = 0 then
                        tokenStart <- i
                        tokenLength <- 1
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
                    if tokenStart < 0 then
                        tokenStart <- i
                | _ -> failwithf "Invalid character '%c' in expression '%s'." character text

                if i = text.Length - 1 && tokenStart >= 0 && tokenLength <= 0 then
                    tokenLength <- 1

                if tokenStart >= 0 && tokenLength > 0 then
                    let token = text.[tokenStart .. tokenStart + tokenLength - 1]

                    match token with
                    | "+" | "*" -> operator <- token.[0]
                    | _ -> expression <- combine expression operator (parseExpression token)

                    tokenStart <- -1
                    tokenLength <- 0

            expression
 
    let parseExpressions lines = 
        [
            for line in lines do
                parseExpression line
        ]