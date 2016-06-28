:- process_create('cat', [], In, Out, Err, Pid),
   write_string(In, foo),
   read_string(Out, 3, S),
   print(S),
   nl.
