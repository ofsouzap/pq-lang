let rec f (x : int) : int =
  if x == 0
  then
    0
  else
    x + f (x - 1)
  end
end

f 5
