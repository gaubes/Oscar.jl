R, _ = QQ["x", "y", "z"]
x = gens(R)[1]
y = gens(R)[2]
z = gens(R)[3]
#f = x^2*(x^2-1)^2
#g = y^2*(y^2-1)^2
#h = z^2*(z^2-1)^2
M = MatrixSpace(R, 2, 3)
F = M([zero(R), x, y, z, x, zero(R)])
#F = F + M(R.([3,0,4,7,7,6]))
F = F + M(R.([0, 1, 0, 0, 0, 0]))
print(F)
I = ideal(R, minors(F, 2))
#I = ideal(R, [f, g, h])
IA2 = Spec(R)
X = subscheme(IA2, I)
W = localized_ring(OO(X))
A = jacobi_matrix(gens(I))


function singular_loci(I::MPolyIdeal)
  d = dim(I)
  R = base_ring(I)
  n = nvars(R)
  X = subscheme(Spec(R), I)
  return degeneracy_locus(X, jacobi_matrix(gens(I)), n-d, 0)
end

slocus_ideal = radical(I + ideal(R, minors(A, 2)))

l = singular_loci(I)

