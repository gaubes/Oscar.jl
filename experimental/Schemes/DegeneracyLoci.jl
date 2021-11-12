import AbstractAlgebra.Generic: MatSpaceElem

export degeneracy_locus

function degeneracy_locus(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    A::MatSpaceElem{RET},
    r::Int, 
    rec_count::Int
  ) where {BRT, BRET, RT, RET, MST1, MST2}
  indent_str = prod([ "#" for i in 1:rec_count ]) * " "
  #println(indent_str * "call with $(X) and matrix $(A) for rank $r")
  X == EmptyScheme(X) && return [EmptyScheme(X)]
  # find the entry with lowest degree
  m = nrows(A)
  n = ncols(A)
  R = base_ring(OO(X))
  if r == 1 
    if m == 0 || n == 0
      #println(indent_str, "zero matrix; returning $X")
      return [X]
    end
    Y = subscheme(X, ideal(R, [A[i,j] for i in 1:nrows(A) for j in 1:ncols(A)]))
    Y == EmptyScheme(Y) && (Y = EmptyScheme(Y))
    #println(indent_str, "zero rank of a non-trivial matrix; returning $Y")
    return [Y]
  end
  (k, l) = (1, 1)
  d = maximum(total_degree.(A))
  I = localized_modulus(OO(X))
  allzero = true
  W = localized_ring(OO(X))
  for i in 1:m
    print_str = indent_str
    for j in 1:n
      A[i,j] = numerator(reduce(W(A[i,j]), groebner_basis(I)))
      print_str *= "$(A[i, j]);\t"
      allzero = allzero && iszero(A[i,j])
      if total_degree(A[i,j]) <= d && !iszero(A[i,j])
	d = total_degree(A[i,j])
	(k, l) = (i, j)
      end
    end
    #println(print_str)
  end
  f = A[k,l]
  if allzero
    #println(indent_str * "All entries are zero; returning $X")
    X == EmptyScheme(X) && (X = EmptyScheme(X))
    return [X]
  end
  #println(indent_str * "selected entry at ($k, $l): $f")
  U = hypersurface_complement(X, f)
  Y = subscheme(X, f)
  B = copy(A)
  for i in 1:k-1
    multiply_row!(B, f, i)
    add_row!(B, -A[i,l], k, i)
  end
  for i in k+1:m
    multiply_row!(B, f, i)
    add_row!(B, -A[i,l], k, i)
  end
  B = vcat(hcat(B[1:k-1, 1:l-1], B[1:k-1, l+1:n]), hcat(B[k+1:m, 1:l-1], B[k+1:m, l+1:n]))
  #println(indent_str * "new matrix of size $(nrows(B)) x $(ncols(B)):")
  for i in 1:nrows(B)
    print_str = indent_str
    for j in 1:ncols(B)
      print_str = print_str * "$(B[i,j]);\t"
    end
    #println(print_str)
  end
  DU = degeneracy_locus(U, B, r-1, rec_count+1)
  DU = [closure(V, X) for V in DU]
  DY = degeneracy_locus(Y, copy(A), r, rec_count+1)
  #@show DY
  return vcat(DU, DY)
end
