-- chapter 04: syntax in functions

-- pattern matching !!!
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry you are out of luck :("

