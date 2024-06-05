% Read file
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    get_char(Stream,X),
    read_file(Stream,L).

% Write the output in the output file
write_compressed_sequence(File, CompressedSequence) :-
    open(File, write, Stream),
    write(Stream, CompressedSequence),
    close(Stream).

% Counting the number of consecutive occurences
count([], []).
count([H|T], [[N, H]|Zs]) :-
    count(T, H, 1, N, Ys),
    count(Ys, Zs).

count([], _, N, N, []).
count([H|T], H, N0, N, Ys) :-
    N1 is N0 + 1,
    count(T, H, N1, N, Ys).
count([H|T], X, N, N, [H|T]) :-
    H \= X.

% Convert to string 
convert_to_string([], []).
convert_to_string([[N, H]|T], [Str|TStr]) :-
    atom_number(AN, N),
    atom_chars(H, [HC]),
    % if-else consition to handle single occurence
    (AN \= '1' -> string_concat("[", AN, S1),
    string_concat(S1, ",", S2),
    string_concat(S2, HC, S3),
    string_concat(S3, "]", Str) ; string_concat("", "", S1),
    string_concat(S1, "", S2),
    string_concat(S2, HC, S3),
    string_concat(S3, "", Str)),
    convert_to_string(T, TStr).

% Main predicate to compress the input
compress_genetic_data(InputFile, OutputFile) :-
    writeln('Reading DNA sequence from input file...'),
    open(InputFile, read, Str1),
    read_file(Str1, List),
    close(Str1),
    writeln('Counting the number of consecutive repetitions...'),
    count(List, CountList),
    convert_to_string(CountList, StrList),
    writeln('Writing the compressed file to OutputFile ...'),
    write_compressed_sequence(OutputFile, StrList).
   
%  Sample predicate to run the program
%  compress_genetic_data('genetic_data.txt', 'compressed_genetic_data.txt').