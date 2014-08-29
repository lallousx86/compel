void example_tokenizer()
{
  compel_tokenizer_t tok;

  tok = compel_tokenize_init("a;b;c;d", ";", "\"", 0);

  size_t pcount = compel_tokenize_parsed_count(tok);

  for (size_t i=0;i<pcount;i++)
  {
    printf("@%d=%s\n", i, compel_tokenize_get(tok, i));
  }

  compel_tokenize_free(tok);
}
