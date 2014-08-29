#ifndef __COMSCRIPT__07052006__
#define __COMSCRIPT__07052006__

#include <windows.h>
#include <string>
#include <stdarg.h>

class com_script_t
{
private:
  IDispatch *_dispatch;
  HRESULT _hr;
  LCID _lcid;
  std::wstring _return_value, _clsid;
  DWORD _clsctx;
public:
  enum consts_e
  {
    DEF_CLSCTX = CLSCTX_INPROC_SERVER,
    DEF_LCID = LOCALE_USER_DEFAULT
  };

  static void string_to_wstring(std::string s, std::wstring &sw);
  static void wstring_to_string(std::wstring sw, std::string &s);

  bool create(char *progid);
  void free();
  com_script_t();
  ~com_script_t();
  bool invoke_sig(char *method, char *sig, char *argv[], int argc);
  bool invoke_fmt(char *method, char *sig, ...);
  inline HRESULT last_result() { return _hr; }
  inline void set_clsctx(DWORD ctx) { _clsctx = ctx; }
  inline void set_lcid(LCID lcid);
  LPCWSTR get_clsid();
  LPCWSTR get_return_value();
};

#endif