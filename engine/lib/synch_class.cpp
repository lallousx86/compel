#include "synch_class.h"

#define CHECK_CREATED_BOOL if (!created()) return false;
bool mutex_class::created()
{
  return _hmutex != 0;
}

bool mutex_class::create()
{
  if (created())
    return true;

  _hmutex = ::CreateMutex(NULL, FALSE, NULL);
  if (_hmutex == 0)
    return false;
  return true;
}

bool mutex_class::create_named()
{
  return false;
}

bool mutex_class::acquire()
{
  CHECK_CREATED_BOOL;
  ::WaitForSingleObject(_hmutex, INFINITE);
  return true;
}

bool mutex_class::release()
{
  CHECK_CREATED_BOOL;
  ::ReleaseMutex(_hmutex);
  return true;
}

bool mutex_class::destroy()
{
  CHECK_CREATED_BOOL;
  ::CloseHandle(_hmutex);
  _hmutex = 0;
  return true;
}

mutex_class::mutex_class()
{
  _hmutex = 0;
}

mutex_class::~mutex_class()
{
  destroy();
}

criticalsection_class::criticalsection_class()
{
  ::InitializeCriticalSection(&_ct);
}

criticalsection_class::~criticalsection_class()
{
  ::DeleteCriticalSection(&_ct);
}

void criticalsection_class::enter()
{
  ::EnterCriticalSection(&_ct);
}

void criticalsection_class::leave()
{
  ::LeaveCriticalSection(&_ct);
}

mutex_scope_protect_class::mutex_scope_protect_class(mutex_class &mutex)
: _mutex_ref(mutex)
{
  _mutex_ref.acquire();
}

mutex_scope_protect_class::~mutex_scope_protect_class()
{
  _mutex_ref.release();
}

criticalsection_scope_protect_class::criticalsection_scope_protect_class(criticalsection_class &crit)
: _critical(crit)
{
  _critical.enter();
}

criticalsection_scope_protect_class::~criticalsection_scope_protect_class()
{
  _critical.leave();
}
