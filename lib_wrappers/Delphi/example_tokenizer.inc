procedure example_tokenizer;
var
  tok: compel_tokenizer_t;
  pcount: Integer;
  i: Integer;
begin
  tok := compel_tokenize_init('a;b;c;d', ';', '\', nil);

  pcount := compel_tokenize_parsed_count(tok);

  for i := 0 to pcount-1 do
  begin
    WriteLn('@', i,'=', compel_tokenize_get(tok, i));
  end;
  
  compel_tokenize_free(tok);

end;

