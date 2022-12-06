-module(aoc_05).
-export([solve/1]).

solve(Name) ->
    {ok, File} = file:open(Name,[read]),
    MovedCrates = process_lines(File, []),
    CrateMover9000 = print_stack_top(lists:nth(1, MovedCrates)),
    CrateMover9001 = print_stack_top(lists:nth(2, MovedCrates)),
    io:format("Crate Mover 9000: ~s\n", [CrateMover9000]),
    io:format("Crate Mover 9001: ~s\n", [CrateMover9001]).

print_stack_top(Stack) -> print_stack_top(Stack, []).
print_stack_top([H | T], Result) ->
    Top = lists:nth(length(H), H),
    print_stack_top(T, Result ++ [Top]);
print_stack_top([], Result) -> Result.

cratemover_9000(From, To, Amount, Crates) -> 
    cratemover_900X(From, To, Amount, 1, Crates).
cratemover_9001(From, To, Amount, Crates) -> 
    cratemover_900X(From, To, Amount, Amount, Crates).

cratemover_900X(_, _, 0, _, Crates) -> Crates;
cratemover_900X(From, To, Amount, Step, Crates) ->
    FromCrates = lists:nth(From, Crates),
    ToCrates = lists:nth(To, Crates),
    CratesToMove = lists:nthtail(length(FromCrates) - Step, FromCrates),
    NewToCrates = ToCrates ++ CratesToMove,
    NewFromCrates = lists:sublist(FromCrates, length(FromCrates) - Step),
    NewCrates1 = lists:sublist(Crates, From - 1) ++ [NewFromCrates] ++ lists:nthtail(From, Crates),
    NewCrates2 = lists:sublist(NewCrates1, To - 1) ++ [NewToCrates] ++ lists:nthtail(To, NewCrates1),
    cratemover_900X(From, To, Amount - Step, Step, NewCrates2).

create_columns([H | T], L) -> create_columns(T, L ++ [[H]]);
create_columns([], L) -> L.

process_crates_row(L, NewRows, _) when length(NewRows) == 0 -> create_columns(L, NewRows);
process_crates_row([H | T], NewRows, Index) ->
    process_crates_row(T, lists:sublist(NewRows, Index) ++ [lists:nth(Index + 1, NewRows) ++ [H]] ++ lists:nthtail(Index + 1, NewRows), Index + 1);
process_crates_row([], NewRows, _) -> NewRows.

crate_rows_to_columns([H | T], NewRows) -> crate_rows_to_columns(T, process_crates_row(H, NewRows, 0));
crate_rows_to_columns([], NewRows) -> NewRows.

clean_columns(Crates) -> clean_columns(Crates, [], 0).
clean_columns([H | T], CleanedColumns, Index) -> clean_columns(T, CleanedColumns ++ [[X || X <- H, X /= 32]], Index + 1);
clean_columns([], CleanedColumns, _) -> CleanedColumns.

parse_crates(L) -> parse_crates(L, []).
parse_crates([$[, C, $] | T], Stack) -> parse_crates(T, Stack ++ [C]);
parse_crates([$\s, $\s, $\s, $\s | T], Stack) -> parse_crates(T, Stack ++ " ");
parse_crates([$\s, _, $\s | T], Stack) -> parse_crates(T, Stack);
parse_crates([$\s | T], Stack) -> parse_crates(T, Stack);
parse_crates([$\n], Stack) -> Stack;
parse_crates([], Stack) -> Stack.

append_crates(Crates, NewCrates) when length(NewCrates) == 0 -> Crates;
append_crates(Crates, NewCrates) -> Crates ++ [NewCrates].

execute_commands(File, Crates9000, Crates9001) ->
    case io:get_line(File, "") of
        eof -> file:close(File),
            [Crates9000, Crates9001];
        Line ->
            Tokens = string:tokens(Line, " \n"),
            Amount = list_to_integer(lists:nth(2, Tokens)),
            From = list_to_integer(lists:nth(4, Tokens)),
            To = list_to_integer(lists:nth(6, Tokens)),
            MovedCrates9000 = cratemover_9000(From, To, Amount, Crates9000),
            MovedCrates9001 = cratemover_9001(From, To, Amount, Crates9001),
            execute_commands(File, MovedCrates9000, MovedCrates9001)
    end.

process_lines(File, Crates) ->
    case io:get_line(File, "") of
        eof -> file:close(File),
            Crates;
        "\n" ->
            RawCrates = crate_rows_to_columns(lists:reverse(Crates), []),
            ProcessedCrates = clean_columns(RawCrates),
            execute_commands(File, ProcessedCrates, ProcessedCrates);
        OneLine -> 
            ParsedCrates = parse_crates(OneLine),
            process_lines(File, append_crates(Crates, ParsedCrates))
        end.

% c(aoc_05).
% aoc_05:solve("input_test.txt").