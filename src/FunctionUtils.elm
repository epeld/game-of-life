module FunctionUtils where


-- Ignores the first argument.
-- Useful for converting a unary function into binary
ignore1 f _ = f


iterate : Int -> (x -> x) -> x -> x
iterate n f = List.foldl (>>) identity (List.repeat n f)
