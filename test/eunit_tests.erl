%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(eunit_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

fst_change(BookShelf0) ->
    %% Get book1 from the shelf
    {ok, Book0} = ecrdt_dict:find(book1, BookShelf0),
    %% Mark book1 as read once
    Book1 = ecrdt_dict:update_counter(read, 1, Book0),
    %% Get bookmarks from book1
    {ok, BookMarks0} = ecrdt_dict:find(bookmarks, Book1),
    %% Remove the bookmark pointing to the page 10
    BookMarks1 = ecrdt_sets:del_element(10, BookMarks0),
    %% Store bookmark changes
    Book2 = ecrdt_dict:store(bookmarks, BookMarks1, Book1),
    %% Set the book name
    Book3 = ecrdt_dict:store(name, ecrdt_reg:assign("fst"), Book2),
    %% Put book1 back on the shelf
    BookShelf1 = ecrdt_dict:store(book1, Book3, BookShelf0),
    %% Remove book2 from the shelf
    BookShelf2 = ecrdt_dict:erase(book2, BookShelf1),
    %% Return the updated bookshelf
    BookShelf2.

snd_change(BookShelf0) ->    
    %% Get book1 from the shelf
    {ok, Book0} = ecrdt_dict:find(book1, BookShelf0),
    %% Mark book1 as read once
    Book1 = ecrdt_dict:update_counter(read, 1, Book0),
    %% Get bookmarks from book1
    {ok, BookMarks0} = ecrdt_dict:find(bookmarks, Book1),
    %% Remove the bookmark pointing to the page 20
    BookMarks1 = ecrdt_sets:del_element(20, BookMarks0),
    %% Store bookmark changes
    Book2 = ecrdt_dict:store(bookmarks, BookMarks1, Book1),
    %% Remove crap from the book1
    Book3 = ecrdt_dict:erase(crap, Book2),
    %% Set the book name
    Book4 = ecrdt_dict:store(name, ecrdt_reg:assign("snd"), Book3),
    %% Put book1 back on the shelf
    BookShelf1 = ecrdt_dict:store(book1, Book4, BookShelf0),
    %% Return the updated bookshelf
    BookShelf1.

incr(Node, Counter) ->
    ecrdt_counter:update(Node, Counter, 1).

decr(Node, Counter) ->
    ecrdt_counter:update(Node, Counter, -1).

counter_update_test() ->
    C0 = ecrdt_counter:new(),
    C1 = incr(node1, C0),
    C2 = incr(node2, C1),
    C3 = incr(node3, C2),
    C4 = decr(node4, C3),
    C5 = decr(node2, C4),
    C6 = incr(node1, C5),
    C7 = decr(node3, C6),
    ?assertEqual(1, ecrdt_counter:value(C7)).

counter_merge_test() ->
    C1 = incr(node1, incr(node2, decr(node1, incr(node2, ecrdt_counter:new())))),
    C2 = incr(node2, incr(node1, decr(node2, incr(node1, ecrdt_counter:new())))),
    C3 = incr(node2, incr(node3, decr(node2, incr(node3, ecrdt_counter:new())))),
    ?assertEqual(4, ecrdt_counter:value(ecrdt_counter:merge([C1, C2, C3]))).

main_test() ->
    Book1 = ecrdt_dict:from_list([{read, ecrdt_counter:new()},
                               {crap, ecrdt_sets:new()},
                               {bookmarks, ecrdt_sets:from_list([10, 20])}]),
    Book2 = ecrdt_dict:from_list([{name, ecrdt_reg:assign("book")},
                               {read, ecrdt_counter:new()},
                               {bookmarks, ecrdt_sets:from_list([10])}]),
    BookShelf0 = ecrdt_dict:from_list([{book1, Book1}, {book2, Book2}]),
    %% Modify concurrently
    BookShelf1 = fst_change(BookShelf0),
    BookShelf2 = snd_change(BookShelf0),
    %% Now merge changes in any crazy order
    BookShelf = ecrdt_dict:merge([BookShelf1, BookShelf2, BookShelf2,
                               BookShelf1, BookShelf2, BookShelf1,
                               BookShelf1, BookShelf2, BookShelf2]),
    %% There shouldn't be book2 on the shelf
    ?assertEqual(error, ecrdt_dict:find(book2, BookShelf)),
    {ok, Book} = ecrdt_dict:find(book1, BookShelf),
    {ok, Read} = ecrdt_dict:find(read, Book),
    {ok, Name} = ecrdt_dict:find(name, Book),
    {ok, BookMarks} = ecrdt_dict:find(bookmarks, Book),
    %% All bookmarks have been removed
    ?assertEqual([], ecrdt_sets:to_list(BookMarks)),
    %% Alas, we're updating counter on the same node, so
    %% this will be 1 instead of 2.
    ?assertEqual(1, ecrdt_counter:value(Read)),
    %% Should no be any crap
    ?assertEqual(error, ecrdt_dict:find(crap, Book)),
    %% Name should be "snd", because snd_change was done later
    ?assertEqual("snd", ecrdt_reg:value(Name)).
