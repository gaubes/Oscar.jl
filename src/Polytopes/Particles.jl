struct CombinatorialPoint{T, V<:AbstractVector{T}} <: AbstractVector{T}
  coordinates::V
end

function size(cp::CombinatorialPoint{T}) where {T}
    return size(cp.coordinates)
end

function getindex(cp::CombinatorialPoint{T}, i::Int) where {T}
    return getindex(cp.coordinates, i)
end

function setindex!(cp::CombinatorialPoint{T}, v, i::Int) where {T}
    return setindex!(cp.coordinates, v, i)
end

function tomatrix(collection)
    if isempty(collection)
        throw(ArgumentError("Empty collection, cannot properly determine size of matrix"))
    end
    return transpose(hcat(collection...))
end

export CombinatorialPoint



struct Ray{T, V<:AbstractVector{T}} <: AbstractVector{T}
  coordinates::V
end

function size(cp::Ray{T}) where {T}
    return size(cp.coordinates)
end

function getindex(cp::Ray{T}, i::Int) where {T}
    return getindex(cp.coordinates, i)
end

function setindex!(cp::Ray{T}, v, i::Int) where {T}
    return setindex!(cp.coordinates, v, i)
end

function tomatrix(collection)
    if isempty(collection)
        throw(ArgumentError("Empty collection, cannot properly determine size of matrix"))
    end
    return transpose(hcat(collection...))
end

export Ray
