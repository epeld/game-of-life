module FunctionUtils where

-- Ignores the first argument.
-- Useful for converting a unary function into binary
ignore1 f _ y = f y
