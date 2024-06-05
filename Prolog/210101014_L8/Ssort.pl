% custom compare function decides the order on the basis of the length of the list
custom_compare(Order, Length1, Length2) :-
    (Length1 < Length2 -> Order = (<)
     ; Order = (>)
    ).

% predicate that compares the sizes of 2 lists
compare_sizes(Order, List1, List2) :-
    length(List1, Length1),
    length(List2, Length2),
    custom_compare(Order, Length1, Length2).

% Predicate that sorts the lists in the order of increasing sizes
sort_and_keep_duplicates_by_size(UnsortedLists, SortedLists) :-
    predsort(compare_sizes, UnsortedLists, TempSortedLists),
    include(\=([]),TempSortedLists,SortedLists).

% Main predicate 
ssort(UnsortedLists,SortedLists) :-
    sort_and_keep_duplicates_by_size(UnsortedLists,SortedLists).

% Sample queries
% ssort([[a,b,c,d],[f,g,h],[f,g,h,e],[a,b,c],[a]],L).
% ssort([[1,2,3],[4,3,2,1],[4,3],[1,2,3,4,5],[9],[2],[4,9,6]],L).
% ssort([[3],[6],[44],[1],[9],[2],[4]],L).
% ssort([[m,n,o],[4,3,2,1],[4,3],[1,2,3,4,5],[9,a,b],[2],[4,9,6]],L).