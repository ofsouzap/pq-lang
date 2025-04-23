# Implementation of an integer-valued polar coordinates type (in degrees)

type ipolar_base = IPolar of (int * int)

qtype ipolar
  = ipolar_base
  |/ (r : int) -> (a : int) => IPolar ((r : int), (a : int)) == (IPolar (r, a + 360))
  |/ (r : int) -> (a : int) => IPolar ((r : int), (a : int)) == (IPolar (r, a - 360))
  |/ (r : int) -> (a : int) => IPolar ((r : int), (a : int)) == (IPolar (-r, a + 180))
  |/ (r : int) -> (a : int) => IPolar ((r : int), (a : int)) == (IPolar (-r, a - 180))

# Rotate a polar coordinate
let rotate (arg : (int * ipolar)) : ipolar =
  match arg -> ipolar with
  | ((x : int), (IPolar ((r : int), (a : int)))) ->
    IPolar (r, (a + x))
  end
end

# Scale the magnitude component of a polar coordinate
let scale (arg : (int * ipolar)) : ipolar =
  match arg -> ipolar with
  | ((x : int), (IPolar ((r : int), (a : int)))) ->
    IPolar (r * x, a)
  end
end
