// dshow_ext.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "dshow_ext.h"

HMODULE g_hInstance = 0;

/*!
\brief lowercase user command
This command takes the first parameter by value and returns the lowercase version
*/
int COMPEL_API ext_dsrender(compel_script_t compel_script, int argc, char *argv[])
{
  IGraphBuilder *pGraph = NULL;    // Graph builder interface
  IMediaControl *pControl = NULL;  // Media control interface
  IMediaEvent   *pEvent = NULL;    // Media event interface

  // Initialize the COM library.
  HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
  if (FAILED(hr))
  {
    return compel_error_symbol_undefined;
  }

  // Create the Filter Graph Manager and query for interfaces.
  hr = CoCreateInstance(CLSID_FilterGraph, NULL, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, (void **)&pGraph);
  if (FAILED(hr))    // FAILED is a macro that tests the return value
  {
    return compel_error_unknown_error;
  }

  // Use IGraphBuilder::QueryInterface (inherited from IUnknown) 
  // to get the IMediaControl interface.
  hr = pGraph->QueryInterface(IID_IMediaControl, (void **)&pControl);
  if (FAILED(hr))
  {
    pGraph->Release();					// Clean up after ourselves
    pGraph = NULL;
    CoUninitialize();  // And uninitialize COM
    return compel_error_unknown_error;
  }

  // And get the Media Event interface, too.
  hr = pGraph->QueryInterface(IID_IMediaEvent, (void **)&pEvent);
  if (FAILED(hr))
  {
    pGraph->Release();    // Clean up after ourselves
    pControl->Release();
    ::CoUninitialize();  // And uninitialize COM
    return compel_error_unknown_error;
  }

  // To build the filter graph, only one call is required.
  // We make the RenderFile call to the Filter Graph Manager
  // to which we pass the name of the media file.
  WCHAR wFileName[MAX_PATH];
  MultiByteToWideChar(CP_ACP, 0, argv[0], -1, wFileName,
    MAX_PATH);
  // This is all that's required to create a filter graph
  // that will render a media file!
  hr = pGraph->RenderFile((LPCWSTR)wFileName, NULL);

  if (SUCCEEDED(hr))
  {
    // Run the graph.
    hr = pControl->Run();
    if (SUCCEEDED(hr))
    {
      // Wait for completion.
      long evCode;
      pEvent->WaitForCompletion(INFINITE, &evCode);
    }

    // And stop the filter graph.
    hr = pControl->Stop();
  }

  // Now release everything and clean up.
  pControl->Release();
  pEvent->Release();
  pGraph->Release();
  CoUninitialize();

  return compel_error_success;
}

int __stdcall compel_ext_init(compel_script_t script, compel_user_context_t *ctx)
{
  int err = 0;

  err = compel_lu_cmd_register2(script, ext_dsrender, "dsrender", 1, 1);

  return err;
}

BOOL APIENTRY DllMain( 
  HMODULE hModule,
  DWORD  ul_reason_for_call,
  LPVOID lpReserved)
{
  switch (ul_reason_for_call)
  {
  case DLL_PROCESS_ATTACH:
    g_hInstance = hModule;
    ::DisableThreadLibraryCalls(hModule);
    break;
  case DLL_THREAD_ATTACH:
  case DLL_THREAD_DETACH:
  case DLL_PROCESS_DETACH:
    break;
  }
  return TRUE;
}