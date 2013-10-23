%% Data type conversions
-define(I2L(I),      integer_to_list(I)).
-define(L2A(I),      list_to_atom(I)).
-define(L2B(L),      list_to_binary(L)).
-define(L2I(L),      list_to_integer(L)).
-define(L2F(L),      list_to_float(L)).
-define(I2B(I),      list_to_binary(integer_to_list(I))).
-define(B2A(B),      binary_to_atom(B, latin1)).
-define(B2A(B, Enc), binary_to_atom(B, Enc)).
-define(B2I(B),      list_to_integer(binary_to_list(B))).
-define(B2L(B),      binary_to_list(B)).
-define(A2B(A),      list_to_binary(atom_to_list(A))).
-define(A2L(A),      atom_to_list(A)).
-define(A2I(A),      list_to_integer(atom_to_list(A))).
-define(F2B(F),      list_to_binary(float_to_list(F))).
-define(IO2B(IO),    iolist_to_binary(IO)).

%% Convert to specified type regardless of input format
-define(TO_B(E),   hydrogen_convert:to_binary(E)).
-define(TO_L(E),   hydrogen_convert:to_list(E)).
-define(TO_A(E),   hydrogen_convert:to_atom(E)).
-define(TO_E_A(E), hydrogen_convert:to_existing_atom(E)).
-define(TO_I(E),   hydrogen_convert:to_integer(E)).
-define(TO_F(E),   hydrogen_convert:to_float(E)).
