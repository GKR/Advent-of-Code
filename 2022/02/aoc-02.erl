-module(aoc_02).
-export([solve/1]).

solve(Name) ->
    {ok, File} = file:open(Name,[read]),
    Sum = next_line(File, 0, 0),
    io:format("Solution: ~w \n", [Sum]).

part_one("A X") -> 1 + 3; % Rock Rock
part_one("A Y") -> 2 + 6; % Rock Paper
part_one("A Z") -> 3 + 0; % Rock Scissors
part_one("B X") -> 1 + 0; % Paper Rock
part_one("B Y") -> 2 + 3; % Paper Paper
part_one("B Z") -> 3 + 6; % Paper Scissors
part_one("C X") -> 1 + 6; % Scissors Rock
part_one("C Y") -> 2 + 0; % Scissors Paper
part_one("C Z") -> 3 + 3. % Scissors Scissors

part_two("A X") -> part_one("A Z"); % Lose
part_two("A Y") -> part_one("A X"); % Draw
part_two("A Z") -> part_one("A Y"); % Win
part_two("B X") -> part_one("B X"); % Lose
part_two("B Y") -> part_one("B Y"); % Draw
part_two("B Z") -> part_one("B Z"); % Win
part_two("C X") -> part_one("C Y"); % Lose
part_two("C Y") -> part_one("C Z"); % Draw
part_two("C Z") -> part_one("C X"). % Win

next_line(File, Sum1, Sum2) ->
    case io:get_line(File, "") of
        eof -> file:close(File),
            {sum1, Sum1, sum2, Sum2};
        OneLine -> 
            io:format(OneLine),
            Line = string:trim(OneLine),
            next_line(File, Sum1 + part_one(Line), Sum2 + part_two(Line))
        end.

% c(aoc_02).
% aoc_02:solve("input.txt").