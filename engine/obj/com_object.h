#ifndef __COMOBJECT__07052006__
#define __COMOBJECT__07052006__

#include <stdio.h>
#include "../object.h"
#include "../lib/com_script.h"

class com_object_t : public object_t
{
  friend class fnc_com;
private:
  value_t *_attr_progid;
  value_t *_attr_clsid;
  value_t *_attr_clsctx;
  value_t *_attr_argc;
  value_t *_attr_callsig;
  value_t *_attr_hresult;
  value_t *_attr_return_value;
  value_t *_attr_ok;

  com_script_t *_com_script;

  void init();

  com_object_t(const com_object_t &rhs) { }
  com_object_t &operator=(const com_object_t &rhs) { }
  static int _count;

public:
  inline com_script_t *com_script() { return _com_script; }
  bool create(char *progid);
  bool invoke(char *method);
  com_object_t();
  virtual ~com_object_t();
};

#endif