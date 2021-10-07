@testset "mpoly-localizations" begin
  R, vars = ZZ["x", "y"]
  f = vars[1]
  I = ideal(R, f)
  S = ComplementOfPrimeIdeal(I)
  W = MPolyLocalRing(R, S)
  M = MPolyLocalRing(R, I)
  a = W(vars[1]//vars[2])
  b = W(vars[2])
  fraction(a)
  parent(b)
  original_ring(parent(b))
  a*b
  a+b
  a-b
  a//b
end


