
-- see if the file exists
function file_exists(file)
    local f = io.open(file, "rb")
    if f then f:close() end
    return f ~= nil
end

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

function pairs_from(line)
    local pairs = {}
    local i = 0
    for p1, p2 in string.gmatch(line, "(%w+),(%w+)") do
        for k, v in string.gmatch(line, "(%w+)-(%w+)") do
            pairs[i] = { min = tonumber(k), max = tonumber(v) }
            i = i + 1
        end
    end
    return pairs
end

-- get all lines from a file, returns an empty 
-- list/table if the file does not exist
function lines_from(file)
    if not file_exists(file) then return {} end
    local lines = {}
    local full_overlapping_ranges = 0
    local partial_overlapping_ranges = 0
    for line in io.lines(file) do 
        lines[#lines + 1] = line
        local pairs = pairs_from(line)
        --print(dump(pairs))
        --print(pairs[0].min)
        if (pairs[0].min >= pairs[1].min and pairs[0].max <= pairs[1].max) 
                or (pairs[1].min >= pairs[0].min and pairs[1].max <= pairs[0].max) then
            --print('pair contains')
            full_overlapping_ranges = full_overlapping_ranges + 1
        end
        --for k, v in string.gmatch(line, "(%w+)-(%w+)") do
            --t[k] = v
            --print('pair[' .. k .. '-' .. v .. ']')
        --end
    end
    print(full_overlapping_ranges)
    return lines
end
  
-- tests the functions above
local file = 'input.txt'
local lines = lines_from(file)
  
-- print all line numbers and their contents
--for k,v in pairs(lines) do
--    print('line[' .. k .. ']', v)
--end
  