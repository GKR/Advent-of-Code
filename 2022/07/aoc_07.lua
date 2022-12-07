function new_dir(name, parent)
    return { name = name, parent = parent, directories = {}, files = {}, size = 0, total_size = 0 }
end

function parse_input(file)
    local fs = new_dir(nil, nil)
    local cwd = fs;
    local is_listing = false
    for line in io.lines(file) do 
        if line:find("^$ cd.*") ~= nil then -- cmd
            is_listing = false
            local dirname = string.match(line, "%s%a+%s(.+)")
            if dirname == '..' then
                cwd = cwd.parent
            else
                if cwd.directories[dirname] == nil then
                    cwd.directories[dirname] = new_dir(dirname, cwd)
                end
                cwd = cwd.directories[dirname]
            end
        elseif line:find("^$ ls") ~= nil then
            is_listing = true
        elseif line:find("(%d+)%s(.+)") ~= nil then
            local name = string.match(line, "[0-9]+%s(.+)")
            local size = string.match(line, "([0-9]+)%s.+")
            cwd.files[#cwd.files + 1] = { name = name, size = size }
        end
    end

    return fs
end

function calculate_size(directory)
    local size = 0
    for i, file in pairs(directory.files) do
        size = size + file.size
    end
    directory.size = size

    local total_size = size
    for dirname, directory in pairs(directory.directories) do
        total_size = total_size + calculate_size(directory)
    end
    directory.total_size = total_size

    return total_size
end

function solve(cwd, output)
    if cwd.total_size > output.required and (cwd.total_size < output.smallest or output.smallest == 0) then
        output.smallest = cwd.total_size
    end

    for dirname, directory in pairs(cwd.directories) do
        if directory.total_size < 100000 then
            output.sum_size_under_100000 = output.sum_size_under_100000 + directory.total_size
        end
        solve(directory, output)
    end
end

local fs = parse_input('input.txt')

calculate_size(fs.directories["/"])

local used = fs.directories["/"].total_size
local required = 30000000 - (70000000 - used)
local output = { sum_size_under_100000 = 0, required = required, smallest = 0 }
solve(fs.directories["/"], output)

print("sum_size_under_100000: " .. output.sum_size_under_100000)
print("smallest: " .. output.smallest)
