list_string_to_binary([], Bin) ->
    Bin;
list_string_to_binary(Items, Bin) ->
    [I|T] = Items,
    list_string_to_binary(T, [write_string(I)|Bin]).
list_string_to_binary(Items) ->
    list_string_to_binary(Items, []).

bin_to_hex(Bin) when is_binary(Bin) ->
    JoinableLength = byte_size(Bin) - 1,
    << Bytes:JoinableLength/binary, LastNibble1:4, LastNibble2:4 >> = Bin,
    [ "<< ",
      [ [ erlang:integer_to_list(Nibble1, 16), erlang:integer_to_list(Nibble2, 16), ", " ]
        || << Nibble1:4, Nibble2:4 >> <= Bytes ],
      erlang:integer_to_list(LastNibble1, 16),
      erlang:integer_to_list(LastNibble2, 16),
      " >>" ].
write_coord(X,Y) ->
    <<X:32/integer-signed-little, Y:32/integer-signed-little>>.
write_int(I) ->
    <<I:32/integer-signed-little>>. % was BE, now LE
write_int_BE(I) ->
    <<I:32/integer-signed-big>>.
write_int_LE(I) ->
    <<I:32/integer-signed-little>>.
write_word(I) ->
    <<I:16/integer-unsigned-little>>. % was BE, now LE
write_byte(B) ->
    <<B:8>>.
write_string(S) when is_atom(S) -> write_string(atom_to_list(S));
write_string(S) when is_list(S) ->
    B = list_to_binary(S),
    Size = size(B),
    [write_word(Size), B];
write_string(S) when is_binary(S) ->
    Size = size(S),
    [write_word(Size), S].
write_term(T) ->
	B = term_to_binary(T),
	[write_int(size(B)),B].

read_byte(Bin) -> % unsigned!
    {B, B1} = split_binary(Bin, 1),
    <<Byte:8>> = B,
    {Byte, B1}.
read_int(Bin) ->
    {B1,B2} = split_binary(Bin, 4),
    <<Int1:32/integer-signed-little>> = B1,
    {Int1, B2}.
read_word(Bin) ->
    {B1,B2} = split_binary(Bin, 2),
    <<Int1:16/integer-unsigned-little>> = B1,
    {Int1, B2}.
read_string(Bin) ->
    {Len, B1} = read_word(Bin),
    {Sbin, B2} = split_binary(B1, Len),
    {binary_to_list(Sbin), B2}.
read_term(Bin) ->
    {Len, B1} = read_int(Bin),
    {Sbin, B2} = split_binary(B1, Len),
    {binary_to_term(Sbin), B2}.
