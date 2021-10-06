@testset "mpoly-localizations" begin
  R, vars = ZZ["x", "y"]
  f = vars[1]
  I = ideal(R, f)
  S = ComplementOfPrimeIdeal(I)
  W = MPolyLocalRing(R, S)
  M = MPolyLocalRing(R, I)
end
