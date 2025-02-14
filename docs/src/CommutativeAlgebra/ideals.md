```@meta
CurrentModule = Oscar
```

```@setup oscar
using Oscar
```

```@contents
Pages = ["ideals.md"]
```

# Ideals in Polynomial Rings

## Constructors

```@docs
ideal(g::Vector{T}) where {T <: MPolyElem}
```

!!! note
    The return types of all constructors above are subtypes of `MPolyIdeal`.


## Gröbner Bases

Algorithmic means to deal with ideals in multivariate polynomial rings are provided by
the concept of Gröbner bases and its workhorse, Buchberger's algorithm for computing
such bases. For both the concept and the algorithm a convenient way of ordering the monomials
appearing in multivariate polynomials and, thus, to distinguish leading terms of such
polynomials is needed.

!!! note
    The performance of Buchberger's algorithm and the resulting Gröbner basis depend crucially on the choice of monomial ordering.


### Monomial Orderings

Given a ring $R$, we write $R[x]=R[x_1, \ldots, x_n]$ for the polynomial ring
over $R$ in the set of variables $x=\{x_1, \ldots, x_n\}$. Monomials in
$x=\{x_1, \ldots, x_n\}$ are written using multi--indices:
If $\alpha=(\alpha_1, \ldots, \alpha_n)\in \N^n$, set
$x^\alpha=x_1^{\alpha_1}\cdots x_n^{\alpha_n}$ and 

$\text{Mon}_n(x) :=  \text{Mon}_n(x_1, \ldots, x_n) := \{x^\alpha \mid \alpha \in \N^n\}.$

A *monomial ordering* on $\text{Mon}_n(x)$  is a total  ordering $>$ on $\text{Mon}_n(x)$ such that

$x^\alpha > x^\beta \Longrightarrow x^\gamma x^\alpha > x^\gamma  x^\beta,
\; \text{ for all }\; \alpha, \beta, \gamma \in \mathbb N^n.$

A monomial ordering $>$ on $\text{Mon}_n(x)$ is called
- *global* if $x^\alpha > 1$ for all $\alpha \not = (0, \dots, 0)$,
- *local* if  $x^\alpha < 1$ for all $\alpha \not = (0, \dots, 0)$, and
- *mixed* if it is neither global nor local.

We then also say that $>$ is a *global* , *local*, or *mixed* *monomial ordering* on $R[x]$.

Some monomial orderings are predefined in OSCAR.

#### Predefined Global Orderings

##### The Lexicographical Ordering

The *lexicographical ordering* `lex` is defined by setting

$x^\alpha > x^\beta \;  \Leftrightarrow \;\exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i > \beta_i.$

##### The Degree Lexicographical Ordering

The *degree lexicographical ordering* `deglex` is defined by setting $\;\deg(x^\alpha) = \alpha_1 + \cdots + \alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \;  \deg(x^\alpha) > \deg(x^\beta)  \;\text{ or }\; \exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i > \beta_i.$

##### The Reverse Lexicographical Ordering

The *reverse lexicographical ordering* `revlex` is defined by setting

$x^\alpha > x^\beta \;  \Leftrightarrow \;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i  > \beta_i.$


##### The Degree Reverse Lexicographical Ordering

The *degree reverse lexicographical ordering* `degrevlex` is defined by setting $\;\deg(x^\alpha) = \alpha_1 + \cdots + \alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \; \deg(x^\alpha) > \deg(x^\beta)  \;\text{ or }\;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i < \beta_i.$


##### Weighted Lexicographical Orderings

If `W` is a vector of positive integers  $w_1, \dots, w_n$, the *weighted lexicographical ordering* `wdeglex(W)`  is defined by setting
$\;\text{wdeg}(x^\alpha) = w_1\alpha_1 + \cdots + w_n\alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \;  \text{wdeg}(x^\alpha) > \text{wdeg}(x^\beta)  \;\text{ or }\; \exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i > \beta_i.$


##### Weighted Reverse Lexicographical Orderings

If `W` is a vector of positive integers  $w_1, \dots, w_n$, the *weighted reverse lexicographical ordering* `wdegrevlex(W)`  is defined by setting
$\;\text{wdeg}(x^\alpha) = w_1\alpha_1 + \cdots + w_n\alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \; \text{wdeg}(x^\alpha) > \text{wdeg}(x^\beta)  \;\text{ or }\;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i < \beta_i.$

#### Predefined Local Orderings

##### The Negative Lexicographical Ordering

The *lnegative exicographical ordering* `neglex` is defined by setting

$x^\alpha > x^\beta \;  \Leftrightarrow \;\exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i < \beta_i.$

##### The Negative Degree Lexicographical Ordering

The *negative degree lexicographical ordering* `negdeglex` is defined by setting $\;\deg(x^\alpha) = \alpha_1 + \cdots + \alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \;  \deg(x^\alpha) < \deg(x^\beta)  \;\text{ or }\; \exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i > \beta_i.$

##### The Negative Reverse Lexicographical Ordering

The *negative reverse lexicographical ordering* `negrevlex` is defined by setting

$x^\alpha > x^\beta \;  \Leftrightarrow \;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i  < \beta_i.$


##### The Negative Degree Reverse Lexicographical Ordering

The *negative degree reverse lexicographical ordering* `negdegrevlex` is defined by setting $\;\deg(x^\alpha) = \alpha_1 + \cdots + \alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \; \deg(x^\alpha) < \deg(x^\beta)  \;\text{ or }\;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i < \beta_i.$



##### Negative Weighted Lexicographical Orderings

If `W` is a vector of positive integers  $w_1, \dots, w_n$, the *negative weighted lexicographical ordering* `negwdeglex(W)`  is defined by setting
$\;\text{wdeg}(x^\alpha) = w_1\alpha_1 + \cdots + w_n\alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \;  \text{wdeg}(x^\alpha) < \text{wdeg}(x^\beta)  \;\text{ or }\; \exists \; 1 \leq i \leq n: \alpha_1 = \beta_1, \dots, \alpha_{i-1} = \beta_{i-1}, \alpha_i > \beta_i.$


##### Negative Weighted Reverse Lexicographical Orderings

If `W` is a vector of positive integers  $w_1, \dots, w_n$, the *negative weighted reverse lexicographical ordering* `negwdegrevlex(W)`  is defined by setting
$\;\text{wdeg}(x^\alpha) = w_1\alpha_1 + \cdots + w_n\alpha_n\;$ and

$x^\alpha > x^\beta \;  \Leftrightarrow \; \text{wdeg}(x^\alpha) < \text{wdeg}(x^\beta)  \;\text{ or }\;\exists \; 1 \leq i \leq n: \alpha_n = \beta_n, \dots, \alpha_{i+1} = \beta_{i+1}, \alpha_i < \beta_i.$


#### Creating Block Orderings

The concept of block orderings allows one to construct new monomial orderings from already given ones: If $>_1$ and $>_2$ are monomial orderings on $\text{Mon}_s(x_1, \ldots, x_s)$ and $\text{Mon}_{n-s}(x_{s+1}, \ldots, x_n)$, respectively, then the *block ordering*
$>=(>_1, >_2)$ on $\text{Mon}_n(x)=\text{Mon}_n(x_1, \ldots, x_n)$ is defined by setting
          
$x^\alpha>x^\beta  \;\Leftrightarrow\;  x_1^{\alpha_1}\cdots x_s^{\alpha_s} >_1 x_1^{\beta_1}\cdots x_s^{\beta_s} \;\text{ or }\;
\bigl(x_1^{\alpha_1}\cdots x_s^{\alpha_s} = x_1^{\beta_1}\cdots x_s^{\beta_s} \text{ and }  x_{s+1}^{\alpha_{s+1}}\cdots x_n^{\alpha_n} >_2
x_{s+1}^{\beta_{s+1}}\cdots x_n^{\beta_n}\bigr).$
          
Note that $>=(>_1, >_2)$ is global (local) iff $>_1$ and $>_2$ are global (local). Mixed orderings arise by choosing
one of $>_1$ and $>_2$ global and the other one local.
		  
#### Creating Matrix Orderings

Given a matrix $M\in \text{GL}(n,\mathbb R)$, with rows $m_1,\dots,m_n$, the *matrix ordering*
defined by $M$ is obtained by setting
         
$x^\alpha>_M x^\beta  \Leftrightarrow  \;\exists\; 1\leq i\leq n:  m_1\alpha=m_1\beta,\ldots, 
m_{i-1}\alpha\ =m_{i-1}\beta,\ m_i\alpha>m_i\beta$

(here, $\alpha$ and $\beta$ are regarded as column vectors).

!!! note
    By a theorem of Robbiano, every monomial ordering arises as a matrix ordering as above.
    

To create matrix orderings, OSCAR allows for matrices with integer coefficients as input matrices.

### Normal Forms

```@docs
normal_form(f::T, J::MPolyIdeal) where { T <: MPolyElem }
```

```@docs
normal_form(A::Vector{T}, J::MPolyIdeal) where { T <: MPolyElem }
```

### Computing Gröbner Bases

```@docs
groebner_basis(I::MPolyIdeal; ordering::Symbol = :degrevlex, complete_reduction::Bool = false)
```

#### Gröbner Bases with transformation matrix

```@docs
groebner_basis_with_transformation_matrix(I::MPolyIdeal; ordering::Symbol = :degrevlex, complete_reduction::Bool=false)
```

    fglm

    Gröbner walks

    Hilbert-driven

#### Leading Ideals

```@docs
leading_ideal(g::Vector{T}, args...) where { T <: MPolyElem }
```

```@docs
leading_ideal(I::MPolyIdeal)
```

#### Gröbner Bases over the integers

#### ....


### Syzygies

#### Generators of syzygies

```@docs
syzygy_generators(a::Vector{<:MPolyElem})
```

## Data Associated to Ideals

```@docs
base_ring(I::MPolyIdeal)
```

### Number of Generators

```@docs
ngens(I::MPolyIdeal)
```

### Generators

```@docs
gens(I::MPolyIdeal)
```

### Dimension

```@docs
dim(I::MPolyIdeal)
```

### Codimension

```@docs
codim(I::MPolyIdeal)
```
    
## Operations on Ideals

### Simple Ideal Operations

#### Powers of Ideal

```@docs
:^(I::MPolyIdeal, m::Int)
```
#### Sum of Ideals

```@docs
:+(I::MPolyIdeal, J::MPolyIdeal)
```

#### Product of Ideals

```@docs
:*(I::MPolyIdeal, J::MPolyIdeal)
```

### Intersection of Ideals

```@docs
intersect(I::MPolyIdeal, J::MPolyIdeal)
```

### Ideal Quotients

Given two ideals $I, J$ of a ring $R$, the ideal quotient of $I$ by $J$ is the ideal

$I:J= \bigl\{f \in R\:\big|\: f J \subset I\bigr\}\subset R.$

```@docs
quotient(I::MPolyIdeal, J::MPolyIdeal)
```

### Saturation

Given two ideals $I, J$ of a ring $R$, the saturation of $I$ with respect to $J$ is the ideal

$I:J^{\infty} = \bigl\{ f \in R \:\big|\: f J^k \!\subset I {\text{ for some }}k\geq 1 \bigr\} = \textstyle{\bigcup\limits_{k=1}^{\infty} (I:J^k)}.$

```@docs
saturation(I::MPolyIdeal, J::MPolyIdeal)
```

```@docs
saturation_with_index(I::MPolyIdeal, J::MPolyIdeal)
```

### Elimination

```@docs
eliminate(I::MPolyIdeal, l::Vector{T}) where T <: Union{MPolyElem, MPolyElem_dec}
```

## Tests on Ideals

### Basic Tests

```@docs
iszero(I::MPolyIdeal)
```

```@docs
isone(I::MPolyIdeal)
```

### Equality of Ideals

```@docs
:(==)(I::MPolyIdeal, J::MPolyIdeal)
```

### Containment of Ideals

```@docs
issubset(I::MPolyIdeal, J::MPolyIdeal)
```

### Ideal Membership

```@docs
ideal_membership(f::T, I::MPolyIdeal) where T <: Union{MPolyElem, MPolyElem_dec}
```

### Radical Membership

```@docs
radical_membership(f::T, I::MPolyIdeal) where T <: Union{MPolyElem, MPolyElem_dec}
```

### Primality Test

```@docs
isprime(I::MPolyIdeal)
```

### Primary Test

```@docs
isprimary(I::MPolyIdeal)
```

## Decomposition of Ideals

We discuss various decomposition techniques. They are implemented for
polynomial rings over fields and, if explicitly mentioned, also for
polynomial rings over the integers. See [DGP99](@cite) for a survey.

### Radical

```@docs
radical(I::MPolyIdeal)
```

### Primary Decomposition

```@docs
primary_decomposition(I::MPolyIdeal)
```

### Minimal Associated Primes

```@docs
minimal_primes(I::MPolyIdeal)
```

### Weak Equidimensional Decomposition

```@docs
equidimensional_decomposition_weak(I::MPolyIdeal)
```

### Equidimensional Decomposition of radical

```@docs
equidimensional_decomposition_radical(I::MPolyIdeal)
```

### Equidimensional Hull

```@docs
equidimensional_hull(I::MPolyIdeal)
```

### Radical of the Equidimensional Hull

```@docs
equidimensional_hull_radical(I::MPolyIdeal)
```

### Absolute Primary Decomposition

```@docs
absolute_primary_decomposition(I::MPolyIdeal{fmpq_mpoly})
```

## Homogenization and Dehomogenization

```@docs
homogenization(f::MPolyElem, var::String, pos::Int=1)
```

###### Examples

```@repl oscar
R, (x, y, z) = PolynomialRing(QQ, ["x", "y", "z"])
f = x^3-y^2-z
F = homogenization(f, "w", 4)
parent(F)
V = [y-x^2, z-x^3]
homogenization(V, "w")
I = ideal(R, V)
PTC = homogenization(I, "w")
parent(PTC[1])
homogenization(I, "w", ordering = :deglex)
```

```@docs
dehomogenization(F::MPolyElem_dec, pos::Int)
```

###### Examples

```@repl oscar
S, (x, y, z) = GradedPolynomialRing(QQ, ["x", "y", "z"])
F = x^3-x^2*y-x*z^2
f = dehomogenization(F, 1)
parent(f)
V = [x*y-z^2, x^2*z-x^3]
dehomogenization(V, 3)
I = ideal(S, V)
dehomogenization(I, 3)
```

	


