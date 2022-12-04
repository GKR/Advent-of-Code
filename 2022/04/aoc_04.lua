
function pairs_from(line)
    local pairs = {}
    local i = 0
    for p1, p2 in string.gmatch(line, "(%w+),(%w+)") do
        for k, v in string.gmatch(line, "(%w+)-(%w+)") do
            pairs[i] = { x1 = tonumber(k), x2 = tonumber(v), width = tonumber(v - k) }
            i = i + 1
        end
    end
    return pairs
end

function is_full_intersection(pair1, pair2)
    return (pair1.x1 >= pair2.x1 and pair1.x2 <= pair2.x2) 
        or (pair2.x1 >= pair1.x1 and pair2.x2 <= pair1.x2)
end

function is_intersection(pair1, pair2)
    return pair1.x2 >= pair2.x1 and pair2.x2 >= pair1.x1
end

function get_overlapping_ranges(file)
    local full_overlapping_ranges = 0
    local partial_overlapping_ranges = 0

    for line in io.lines(file) do 
        local pairs = pairs_from(line)
        if is_full_intersection(pairs[0], pairs[1]) then
            full_overlapping_ranges = full_overlapping_ranges + 1
        end
        if is_intersection(pairs[0], pairs[1]) then
            partial_overlapping_ranges = partial_overlapping_ranges + 1
        end
    end

    return {
        full_overlapping_ranges = full_overlapping_ranges, 
        partial_overlapping_ranges = partial_overlapping_ranges
    }
end

local overlapping_ranges = get_overlapping_ranges('input.txt')

-- Just to dump a table to string
function dump(o)
    if type(o) == 'table' then
       local s = '{ '
       for k,v in pairs(o) do
          if type(k) ~= 'number' then k = '"'..k..'"' end
          s = s .. '['..k..'] = ' .. dump(v) .. ','
       end
       return s .. '} '
    else
       return tostring(o)
    end
end 

print(dump(overlapping_ranges))
