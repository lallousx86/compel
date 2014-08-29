int value_test()
{
  value_t t(123);

  t = 111;

  printf("%s\n", t.get_str_value(8));

  return 0;
}

int object_test()
{
  object_t obj;

  value_t *size = obj.insert_attribute("size");
  size->set_int_value(1234);

  printf("size=%s\n", obj["size"].get_str_value());

  return 0;
}

void test_lines()
{
  lines_t line;

  for (int i=0;i<10;i++)
  {
    char str[100];
    sprintf(str, "line #%d", i+1);
    line.add(str);
  }

  printf("%s\n", line.getline(10));
}


class fnc_about : public function_t
{
private:
  void basic_init()
  {
    set_namedesc("about", "Prints about text!");
    set_minmaxargs(0, 0);
  }

public:
  fnc_about(interpreter_t *interpreter)
  {
    basic_init();
    set_interpreter(interpreter);
  }

  fnc_about()
  {
    basic_init();
  }

  parse_errors_e execute()
  {
    variable_t *temp = 0;
    printf("This is about text!\n");
    return parse_error_none;
  }
};

void test_symtbl()
{
  symbol_table_t st;

  value_t *pval;
  variable_t *pvar;
  object_t *pobj;
  function_t *pfnc;

  object_t *obj1 = new object_t();

  // add an attribute to this object
  obj1->insert_attribute("val1000")->set_int_value(1000);
  obj1->insert_attribute("hello")->set_str_value("hello");

  // add a normal variable
  st.add_symbol("v1", new value_t(1234));

  // add an object variable
  st.add_symbol("obj1", obj1);

  // add a function symbol
  st.add_symbol("about", new fnc_about());

  // retrieve the variable from the symbol table
  pvar = static_cast<variable_t *>(st.find_symbol("v1"));
  pval = static_cast<value_t *>(pvar);
  printf("value of v1=%d\n", pval->get_int_value());

  // retrieve the object from the symbol table
  pvar = static_cast<variable_t *>(st.find_symbol("obj1"));
  pobj = static_cast<object_t *>(pvar);

  printf("obj.hello=%s obj.val1000=%d\n", pobj->value("hello")->get_str_value(), 
    pobj->value("val1000")->get_int_value());

  pfnc = static_cast<function_t *>(st.find_symbol("about"));
  pfnc->execute();
}


void test_parser1(interpreter_t *parser = 0)
{
  if (parser == 0)
    parser = new interpreter_t();

  symbol_table_t st;

  function_t *cmd_about = new fnc_about(parser);
  function_t *cmd_echo = new fnc_echo(parser);
  object_t *obj1 = new object_t();
  object_t *obj2 = new object_t();

  // setup - obj1
  obj1->insert_attribute("val1000")->set_int_value(1000);
  obj1->insert_attribute("hello")->set_str_value("hello");
  obj1->insert_attribute("name")->set_str_value("$obj1");

  // setup - obj2
  obj2->insert_attribute("age")->set_str_value("a hundred");

  // add variables
  st.add_symbol("$v1", new value_t(1234));

  // add an objects variables
  st.add_symbol("$obj1", obj1);
  st.add_symbol("$obj2", obj2);

  // add a function symbol
  st.add_symbol("about", cmd_about);
  st.add_symbol("echo", cmd_echo);

  std::string line;

  parser->set_symbol_table(&st);

  line = "echo elias says: $obj1.hello $obj2.age $obj1.val1000 $v1";

  parser->interpret_new_line(line.c_str());

  delete parser;
}

void test_file_object1()
{
  char buf[1024] = {0};

  file_object_t fo("_trash.txt", "r");

  if (!fo.open())
    return;

  while (fo["eof"].get_int_value() == 0)
  {
    fo.read(buf, 100);
    printf("name: %s;pos:%d;read:%d\nbuf:%s<\n", 
      fo["filename"].get_str_value(),
      fo["pos"].get_int_value(),
      fo["read"].get_int_value(),
      buf);
  }
  fo.close();
}

void test_memory_object()
{
  memory_object_t mo(1024);

  mo.alloc();

  strcpy(mo.get_ptr(), "Hello world!");
  printf("mo; size=%d ptr=%p; -> %s\n", 
    mo["size"].get_int_value(), 
    mo["ptr"].get_int_value(),
    mo.get_ptr());

  //mo.free();
}
