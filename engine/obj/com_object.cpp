#include "com_object.h"

static const char
  *s_progid = "progid",
  *s_argc = "argc",
  *s_callsig = "callsig",
  *s_hresult = "hresult",
  *s_return_value = "return_value",
  *s_clsid = "clsid",
  *s_clsctx = "clsctx",
  *s_ok = "ok";

int com_object_t::_count = 0;

// called only from ctor
void com_object_t::init()
{
  // set the object's attributes
  _attr_progid        = insert_attribute(s_progid);
  _attr_argc          = insert_attribute(s_argc);
  _attr_callsig       = insert_attribute(s_callsig);
  _attr_hresult       = insert_attribute(s_hresult);
  _attr_return_value  = insert_attribute(s_return_value);
  _attr_clsid         = insert_attribute(s_clsid);
  _attr_clsctx        = insert_attribute(s_clsctx, value_t(com_script_t::DEF_CLSCTX));
  _attr_ok            = insert_attribute(s_ok, value_t("0"));

  _com_script = 0;
  set_obj_kind(object_com);
}

com_object_t::com_object_t()
{
  init();
}

bool com_object_t::create(char *progid)
{
  HRESULT hr = ::CoInitialize(NULL);
  _attr_hresult->set_int_value(hr);
  if (!SUCCEEDED(hr) && (hr != CO_E_ALREADYINITIALIZED))
      return false;

  // delete previous com_script
  delete _com_script;

  // Increment instances count
  _count++;

  com_script_t *inst = new com_script_t;
  
  inst->set_clsctx((DWORD)_attr_clsctx->get_int_value());

  bool bCreate = inst->create(progid);

  _attr_hresult->set_int_value(inst->last_result());
  _attr_ok->set_int_value(bCreate ? 1 : 0);

  if (!bCreate)
  {
    delete inst;
    return false;
  }

  _com_script = inst;

  std::string s;
  com_script_t::wstring_to_string(_com_script->get_clsid(), s);

  _attr_clsid->set_str_value(s.c_str());

  return true;
}


com_object_t::~com_object_t()
{
  delete _com_script;
}

bool com_object_t::invoke(char *method)
{
  if (_com_script == 0)
    return false;

  // get arguments count
  int argc = _attr_argc->get_int_value();

  // allocate parameters
  char **argv = new char *[argc];

  int i;

  // Find attributes and convert to parameters
  for (i=0;i<argc;i++)
  {
    char attr_name[15];
    sprintf(attr_name, "%d", i);
    value_t *arg = find_attribute(attr_name);

    // attribute not set? create an empty one
    if (arg == 0)
      arg = insert_attribute(attr_name);

    // get argument value pointer
    const char *arg_str = arg->get_str_value();

    // get argument value length
    size_t arg_len = strlen(arg_str);

    // allocate memory
    argv[i] = new char[ arg_len + 1];

    // copy argument from object attribute
    strcpy(argv[i], arg_str);
  }

  // invoke the method
  _attr_ok->set_int_value(
    _com_script->invoke_sig(method, (char *) _attr_callsig->get_str_value(), argv, argc) ? 1 : 0
  );

  // return HRESULT
  _attr_hresult->set_int_value(_com_script->last_result());

  // return method's return value
  std::string s;
  com_script_t::wstring_to_string(_com_script->get_return_value(), s);
  _attr_return_value->set_str_value(s.c_str());

  // Free parameters
  for (i=0;i<argc;i++)
    delete [] argv[i];

  delete [] argv;

  return true;
}