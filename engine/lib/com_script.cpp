#include "com_script.h"

/*
07/05/2006
- Initial release
*/

LPCWSTR com_script_t::get_return_value()
{
  return _return_value.c_str();
}

void com_script_t::string_to_wstring(std::string s, std::wstring &sw)
{
  sw = L"";

  DWORD nLen = MultiByteToWideChar(CP_ACP, 0, s.c_str(), (int) s.size(), 0, 0);
  if (nLen == 0)
    return;

  WCHAR *buf = new WCHAR[nLen+1];
  buf[nLen] = UNICODE_NULL;

  MultiByteToWideChar(CP_ACP, 0, s.c_str(), (int) s.size(), buf, nLen);

  sw = buf;

  delete [] buf;
}

void com_script_t::wstring_to_string(std::wstring sw, std::string &s)
{
  BOOL bBool = FALSE;
  DWORD nLen = WideCharToMultiByte(CP_ACP, 0, sw.c_str(), (int) sw.size(), 0, 0, 0, &bBool);
  if (nLen <= 0)
    return;

  char *buf = new char[nLen + 1];
  buf[nLen] = '\0';

  WideCharToMultiByte(CP_ACP, 0, sw.c_str(), (int) sw.size(), buf, nLen, 0, &bBool);

  s = buf;

  delete [] buf;
}


inline void com_script_t::set_lcid(LCID lcid)
{
  _lcid = lcid;
}

com_script_t::~com_script_t()
{
  free();
}

com_script_t::com_script_t()
{
  _dispatch = 0;
  _lcid = DEF_LCID;
  _clsctx = DEF_CLSCTX;
}

void com_script_t::free()
{
  if (_dispatch == 0)
    return;
  _dispatch->Release();
  _dispatch = 0;
}

LPCWSTR com_script_t::get_clsid()
{
  return _clsid.c_str();
}

bool com_script_t::create(char *progid)
{
  free();

  GUID clsid;
  std::wstring ws;

  string_to_wstring(progid, ws);
  _hr = CLSIDFromProgID(ws.c_str(), &clsid);

  if (!SUCCEEDED(_hr))
    return false;

  LPOLESTR olestr = 0;
  _hr = StringFromCLSID(clsid, &olestr);
  if (!SUCCEEDED(_hr))
    return false;

  _clsid = olestr;

  CoTaskMemFree(olestr);

  _hr = ::CoCreateInstance(clsid, NULL, _clsctx, IID_IDispatch, (LPVOID *)&_dispatch);
  if (!SUCCEEDED(_hr))
    return false;

  return true;
}

bool com_script_t::invoke_sig(char *method, char *sig, char *argv[], int argc)
{
  static const char id_str = 's';
  static const char id_int = 'i';

  int i, j;

  std::wstring ws;

  // convert method name
  string_to_wstring(method, ws);

  //
  // Get DISPID
  //

  DISPID dispid = 0;
  LPOLESTR methodName = (LPOLESTR) ws.c_str();

  _hr = _dispatch->GetIDsOfNames(IID_NULL, &methodName, 1, _lcid, &dispid);
  if (!SUCCEEDED(_hr))
    return false;

  //
  // Form parameters
  //

  DISPPARAMS params = {0};

  params.cArgs = argc;
  params.rgvarg = new VARIANTARG[argc];

  // in reverse order
  for (i=argc - 1,j = 0;i>=0;i--,j++)
  {
    VARIANTARG &param = params.rgvarg[j];

    // string?
    if (sig[i] == id_str)
    {
      param.vt = VT_BSTR;
      string_to_wstring(argv[i], ws);
      param.bstrVal = SysAllocString(ws.c_str());
    }
    else if (sig[i] == id_int)
    {
      param.lVal = atoi(argv[i]);
      param.vt = VT_I4;
    }
  }

  EXCEPINFO excep = {0};
  VARIANTARG varResult = {0};
  UINT argerr = (UINT)-1;

  // Call the method
  _hr = _dispatch->Invoke(dispid, IID_NULL, _lcid, DISPATCH_METHOD, &params, &varResult, &excep, &argerr);

  if (_hr == DISP_E_MEMBERNOTFOUND)
  {
    _hr = _dispatch->Invoke(dispid, IID_NULL, _lcid, DISPATCH_METHOD, &params, NULL, &excep, &argerr);
  }

  // Free BSTRs
  for (i=argc - 1,j = 0;i>=0;i--,j++)
  {
    VARIANTARG &param = params.rgvarg[j];
    if (sig[i] == id_str)
    {
      SysFreeString(param.bstrVal);
    }
  }

  // Delete arguments
  delete [] params.rgvarg;

  if (SUCCEEDED(VariantChangeType(&varResult, &varResult, 0, VT_BSTR)))
    _return_value = varResult.bstrVal;
  else
    _return_value = L"";

  return true;
}

bool com_script_t::invoke_fmt(char *method, char *sig, ...)
{
  va_list va;

  int argc = (int) strlen(sig);

  va_start(va, sig);
  char **argv = new char *[argc];

  int i = 0;
  for (i=0;i<argc;i++)
  {
    char *p = va_arg(va, char *);
    argv[i] = new char[ strlen(p) + 1];
    strcpy(argv[i], p);
  }

  invoke_sig(method, sig, argv, argc);

  // Delete each argument
  for (i=0;i<argc;i++)
    delete [] argv[i];

  // Delete array
  delete [] argv;

  va_end(va);

  return true;
}
