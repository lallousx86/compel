#ifndef _COMPEL_DEBUG_SCOPE
#include "fnc_scopes.h"
#include "fnc_branching.h"
#include "../interpreter_helper.h"
#include "fnc_usercommand.h"
#include <stack>
#endif

enum SCOPE_CONSTS
{
  // preparse error codes
  NOERRORS = 0,
  IMBALANCE,
  WRONGCONTEXT,
  SCOPENEEDED,
  PARAM_LESS,
  PARAM_MORE,
  VARNAME_NEEDED,

  // binding types
  OCCUPY_NONE,
  OCCUPY_IF,
  OCCUPY_FNC,
  OCCUPY_FOR
};

static const char 
  *s_cmd_if = "if",
  *s_cmd_scope_begin = "{",
  *s_cmd_scope_end = "}",
  *s_cmd_else = "else",
  *s_cmd_continue = "continue",
  *s_cmd_break = "break",
  *s_cmd_command = "command",
  *s_cmd_for = "for",
  *s_cmd_while = "while",
  *s_cmd_dowhile = "dowhile",
  *s_cmd_return = "return",
  *s_to = "to",
  *s_downto = "downto",
  *s_cmd_if_xx = "if_",

  // messages / misc.
  *s_for_dir_fwd = "to",
  *s_for_dir_prev = "downto",
  *s_for_err_nodir = "for loop specified with wrong direction. expected TO or DOWNTO";

static const int PAR_SCOPEID = 1;
static const int PAR_IF_XX_LABEL = 3;

namespace compel_fnc_scopes
{
  struct scope_binding_t
  {
    int bind_type;
    virtual ~scope_binding_t()
    {
    }
  };

  struct scope_command_binding_t : scope_binding_t
  {
    scope_command_binding_t()
    {
      bind_type = OCCUPY_FNC;
    }
  };

  struct scope_if_binding_t : scope_binding_t
  {
    size_t at_after_condition;
    scope_if_binding_t()
    {
      bind_type = OCCUPY_IF;
    }
  };

  struct scope_for_binding_t : scope_binding_t
  {
    long bound1;
    long bound2;
    long direction;
    bool broken;
    value_t *ctrl_var;
    std::string ctrl_var_name;
    scope_for_binding_t()
    {
      bind_type = OCCUPY_FOR;
      broken = false;
    }
  };

}

using namespace compel_fnc_scopes;

static int preparse_scopes(lines_t &lines, scope_preparse_t &preparse);

static bool is_cmd_if(const char *cmd)
{
  return stricmp(cmd , s_cmd_if) == 0;
}

static bool is_cmd_continue(const char *str)
{
  return stricmp(str, s_cmd_continue) == 0;
}

static bool is_cmd_if_xx(const char *str)
{
  return strnicmp(str, s_cmd_if_xx, 3) == 0;
}

static bool is_cmd_command(const char *str)
{
  return stricmp(str, s_cmd_command) == 0;
}

static bool is_cmd_break(const char *str)
{
  return stricmp(str, s_cmd_break) == 0;
}

static bool is_cmd_return(const char *str)
{
  return stricmp(str, s_cmd_return) == 0;
}

#ifndef _COMPEL_DEBUG_SCOPE
scope_context_map_t &fnc_scopes::_scopes_map = fnc_scopes::_preparse.scopes;
scope_preparse_t fnc_scopes::_preparse;

#endif

static int preparse_scopes(lines_t &lines, scope_preparse_t &preparse)
{
  scope_context_map_t     &scopes_map = preparse.scopes;
  int                     &failing_line = preparse.start_or_failing_line;

  scope_context_stack_t    stk_scope;
  if_context_stack_t       stk_if;
  ufnc_context_stack_t     stk_ufnc;
  for_context_stack_t      stk_for;

  scope_context_list_t     patch_scope;
  if_context_list_t        patch_if;
  ufnc_context_list_t      patch_ufnc;
  for_context_list_t       patch_for;
  brkcnt_context_map_t     patch_continue, patch_break, patch_return;


  // clear command list
  preparse.commands.clear();

  char temp[1024];

  size_t nlines = lines.count();

  static size_t nscope = 0;

  compel_string_tokenizer_t slp;

  for (size_t i=preparse.start_or_failing_line;i<nlines;i++)
  {
    bool b_cmd_continue(false), b_cmd_break(false), b_cmd_return(false), b_cmd_if_xx(false);

    failing_line = (int) i;

    std::string &line = lines.line(i);

#ifdef _COMPEL_DEBUG_SCOPE    
    printf("@%0.3d: %s\n", i+1, line.c_str());
#endif

    size_t pcount = slp.parse((char *)line.c_str());
    if (pcount == 0)
      continue;

    const char *cmd = slp.get_string(0);

    // Scope BEGIN
    if (stricmp(cmd, s_cmd_scope_begin) == 0)
    {
      // get scope info
      scope_context_t ctx_scope;
      ctx_scope.level = stk_scope.size() + 1;
      ctx_scope.begin = i;
      ctx_scope.end = 0;
      ctx_scope.id = ++nscope;

      if_context_t    *top_if = 0;
      ufnc_context_t  *top_ufnc = 0;
      for_context_t   *top_for = 0;

      if (stk_if.size())
        top_if = &stk_if.top();

      if (stk_ufnc.size())
        top_ufnc = &stk_ufnc.top();

      if (stk_for.size())
        top_for = &stk_for.top();

      if (
        (top_if && 
        top_ufnc && 
        (top_if->level == top_ufnc->level)
        ) 
        ||
        (top_if && 
        top_for && 
        (top_if->level == top_for->level))
        )
      {
        // IF was not even bound to the TRUE scope?
        if (top_if->end1 == 0)
        {
          return WRONGCONTEXT;
        }

        // IF was bound, give priority to other scopes
        // IF will be closed automatically later
        top_if = 0;
      }

#ifdef _COMPEL_DEBUG_SCOPE    
      printf("%s", identstr(ctx_scope.level));
#endif
      // bound to an IF ?
      if (top_if != 0 && top_if->level == ctx_scope.level)
      {
        // not already bound?
        if (top_if->begin1 == 0)
        {
          top_if->begin1   = ctx_scope.begin;
          top_if->scope_id1 = ctx_scope.id;
#ifdef _COMPEL_DEBUG_SCOPE    
          printf("if begin id=%d level=%d\n", top_if->scope_id1, top_if->level);
#endif
        }
        // an ELSE is present
        else if (top_if->else_line != 0 && top_if->begin2 == 0)
        {
          top_if->begin2 = ctx_scope.begin;
          top_if->scope_id2 = ctx_scope.id;

          if ( 
            ((top_ufnc != 0) && (top_ufnc->level == top_if->level)) 
            ||
            ((top_for != 0) && (top_for->level == top_if->level)) 
            )
          {
            return WRONGCONTEXT;
          }
#ifdef _COMPEL_DEBUG_SCOPE    
          printf("else begin id=%d level=%d\n", top_if->scope_id1, top_if->level);
#endif
        }
        else
        {
          // already bound and a new IF was initiated
          // so we need to close the last IF
#ifdef _COMPEL_DEBUG_SCOPE    
          printf("if end(1) scopeid=%d level=%d\n", top_if->scope_id1, top_if->level);
#endif
          patch_if.push_back(*top_if);
          stk_if.pop();
        }
      }
      // bound to a FUNCTION?
      else if (top_ufnc && top_ufnc->level == ctx_scope.level)
      {
        // bind the scope to the function
        ctx_scope.binding = new scope_command_binding_t;

        // pass SCOPEID
        top_ufnc->scope_id = ctx_scope.id;
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("%sfunction_begin id=%d level=%d\n", identstr(top_ufnc->level), top_ufnc->scope_id, top_ufnc->level);
#endif
      }
      // bound to a FOR loop?
      else if (top_for && top_for->level == ctx_scope.level)
      {
        top_for->scope_id = ctx_scope.id;
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("%sfor_begin id=%d level=%d\n", identstr(top_for->level), top_for->scope_id, top_for->level);
#endif
      }

      // stack scope
      stk_scope.push(ctx_scope);
    }
    // Scope END
    else if (stricmp(cmd, s_cmd_scope_end) == 0)
    {
      if (stk_scope.size() == 0)
      {
        return IMBALANCE;
      }

      scope_context_t *top = &stk_scope.top();
      top->end = i;

      ufnc_context_t *top_ufnc = 0;
      if (stk_ufnc.size())
        top_ufnc = &stk_ufnc.top();

      for_context_t *top_for = 0;
      if (stk_for.size())
        top_for = &stk_for.top();

      if (top_ufnc && top_ufnc->level == top->level)
      {
        //top_ufnc->scope_id = top->id;
        patch_ufnc.push_back(*top_ufnc);
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("%sfunction_end id=%d level=%d\n", identstr(top_ufnc->level), top_ufnc->scope_id, top_ufnc->level);
#endif
        stk_ufnc.pop();
      }
      else if (top_for && top_for->level == top->level)
      {
        patch_for.push_back(*top_for);
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("%sfor_end id=%d level=%d\n", identstr(top_for->level), top_for->scope_id, top_for->level);
#endif
        stk_for.pop();
      }
      else
      {
        while (stk_if.size())
        {
          if_context_t *top_if = &stk_if.top();

          if (top_if->level == top->level)
          {
            // no end to IF, mark it as a probable end
            // perhaps there will be an ELSE somewhere next
            if (top_if->end1 == 0)
            {
              top_if->end1 = top->end;
#ifdef _COMPEL_DEBUG_SCOPE    
              printf("%sif ~end(1) id=%d level=%d\n", identstr(top_if->level), top_if->scope_id1, top_if->level);
#endif
              break;
            }
            // an ELSE was tagged?
            // we should close this IF
            else if (top_if->begin2 != 0)
            {
              top_if->end2 = top->end;
#ifdef _COMPEL_DEBUG_SCOPE    
              printf("%sif else end(1) id=%d level=%d\n", identstr(top_if->level), top_if->scope_id2, top_if->level);
#endif
              patch_if.push_back(*top_if);
              stk_if.pop();
              break;
            }
          }
          // scope lessened, thus we assume an end to the if
          else if (top_if->level > top->level)
          {
#ifdef _COMPEL_DEBUG_SCOPE    
            printf("%sif end(3) id=%d level=%d\n", identstr(top_if->level), top_if->scope_id1, top_if->level);
#endif
            patch_if.push_back(*top_if);
            stk_if.pop();
          }
          else
            break;
        }
      }
      patch_scope.push_back(*top);
      stk_scope.pop();
    }

    //
    // IF
    //
    else if (is_cmd_if(cmd))
    {
      if (pcount < 4)
      {
        return PARAM_LESS;
      }

      if_context_t ctx_if;

      ctx_if.opr1 = slp.get_string(1, true);
      ctx_if.op   = slp.get_string(2, true);
      ctx_if.opr2 = slp.get_string(3, true);

      ctx_if.begin1 = ctx_if.end1 = 0;
      ctx_if.begin2 = ctx_if.end2 = 0;
      ctx_if.level = stk_scope.size() + 1;
      ctx_if.scope_id1 = ctx_if.scope_id2 = 0;
      ctx_if.if_line = i;
      ctx_if.else_line = 0;
      stk_if.push(ctx_if);
    }
    //
    // ELSE
    //
    else if (stricmp(cmd, s_cmd_else) == 0)
    {
      if (stk_if.size() == 0)
      {
        // ELSE used without IF
        return WRONGCONTEXT;
      }

      if_context_t    *top_if    = &stk_if.top();
      scope_context_t *top_scope = 0;

      size_t scope_level, if_level = top_if->level - 1;

      if (stk_scope.size())
      {
        top_scope   = &stk_scope.top();
        scope_level = top_scope->level;
      }
      else
        scope_level = 0;

      // else should be used @ the same level of an IF
      if (if_level != scope_level)
      {
        //die("ELSE used in wrong context scope!");
        return WRONGCONTEXT;
      }
      else if (top_if->scope_id1 == 0)
      {
        // the TRUE (first) scope wasn't even defined!
        return WRONGCONTEXT;
      }
      top_if->else_line = i;
    }

    //
    // C O N T I N U E  or  B R E A K  or  R E T U R N 
    //
    else if ( 
      (b_cmd_continue = is_cmd_continue(cmd))
      ||
      (b_cmd_break = is_cmd_break(cmd))
      ||
      (b_cmd_return = is_cmd_return(cmd))
      || 
      (b_cmd_if_xx = ((pcount == 4 || pcount == 5) && is_cmd_if_xx(cmd)) )
      )
    {
      bool bScopeThere = stk_scope.size() != 0;

      scope_context_t *top_scope = 0;
      size_t scopeid = 0;

      if (bScopeThere)
      {
        top_scope = &stk_scope.top();
        scopeid = top_scope->id;
      }

      brkcnt_context_t ctx;
      ctx.cmd_line = i;
      ctx.level = 0;

      // parse additional parameter passed to break/continue
      if (b_cmd_if_xx == false)
      {
        size_t brk_level_idx;
        brk_level_idx = 2;
        if (b_cmd_return == false && pcount >= brk_level_idx)
          ctx.level = compel_string_tokenizer_t::parse_number(slp.get_string(brk_level_idx-1));
      }

      if (b_cmd_if_xx)
      {
        const char *ifxx_act = slp.get_string(PAR_IF_XX_LABEL);
        bool bPatch = true;

        if (is_cmd_return(ifxx_act))
        {
          if (stk_ufnc.size() == 0)
            return SCOPENEEDED;

          scopeid = stk_ufnc.top().scope_id;
          sprintf(temp, "%s %d", s_cmd_return, scopeid);
        }
        else if (is_cmd_break(ifxx_act))
          sprintf(temp, "%s %d", s_cmd_break, scopeid);
        else if (is_cmd_continue(ifxx_act))
          sprintf(temp, "%s %d", s_cmd_continue, scopeid);
        else 
          bPatch = false;

        // patch directly
        if (bPatch && !bScopeThere)
          return SCOPENEEDED;
        else if (bPatch)
        {
          slp.set_string(PAR_IF_XX_LABEL, temp);
          lines.line(i) = slp.join(" ");
        }
      }
      else if (!bScopeThere)
      {
        return SCOPENEEDED;
      }
      else if (b_cmd_continue)
      {
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("%scontinue: id=%d\n", identstr(top_scope->level), top_scope->id);
#endif
        patch_continue.insert(brkcnt_context_map_t::value_type(scopeid, ctx));
      }
      else if (b_cmd_break)
      {
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("break: id=%d\n", top_scope->id);
#endif
        patch_break.insert(brkcnt_context_map_t::value_type(scopeid, ctx));
      }
      else if (b_cmd_return)
      {
        if (stk_ufnc.size() == 0)
        {
          return SCOPENEEDED;
        }
        ctx.misc = slp.join_from(1);

        // get function's scope ID
        scopeid = stk_ufnc.top().scope_id;
#ifdef _COMPEL_DEBUG_SCOPE    
        printf("return: id=%d\n", scopeid);
#endif
        patch_return.insert(brkcnt_context_map_t::value_type(scopeid, ctx));
      }
    }
    //
    // F U N C T I O N
    //
    else if (is_cmd_command(cmd))
    {
      if (pcount < 2)
      {
        return PARAM_LESS;
      }

      ufnc_context_t ctx;
      ctx.fnc_name = slp.get_string(1);
      ctx.fnc_param = slp.join_from(2);

      ctx.scope_id = 0;
      ctx.fnc_line = i;
      ctx.level = stk_scope.size() + 1;
      stk_ufnc.push(ctx);
    }
    //
    //  F O R   L O O P 
    //
    else if (stricmp(cmd, s_cmd_for) == 0)
    {
      if (pcount < 5)
      {
        return PARAM_LESS;
      }

      for_context_t ctx;
      ctx.control_var = slp.get_string(1);
      ctx.bound1      = slp.get_string(2);
      ctx.direction   = slp.get_string(3);
      ctx.bound2      = slp.get_string(4);

      ctx.scope_id = 0;
      ctx.for_line = i;
      ctx.level = stk_scope.size() + 1;
      stk_for.push(ctx);
    }
  }

  // 
  // PATCHING
  //
  while (stk_if.size())
  {
    if_context_t &top_if = stk_if.top();
    patch_if.push_back(top_if);
#ifdef _COMPEL_DEBUG_SCOPE    
    printf("@@@@: if end(1) id=%d level=%d\n", top_if.scope_id1, top_if.level);
#endif
    stk_if.pop();
  }

  // Patch SCOPES
  for (scope_context_list_t::iterator it_ctx = patch_scope.begin(); it_ctx != patch_scope.end(); ++it_ctx)
  {
    brkcnt_context_map_t::iterator bc_itL, bc_itH;

#ifdef _COMPEL_DEBUG_SCOPE    
    char *ident = identstr(it_ctx->level-1);
#endif

    // Save this scope
    scopes_map[it_ctx->id] = *it_ctx;

    // Found patches for continue?
    bc_itL = patch_continue.lower_bound(it_ctx->id);
    bc_itH = patch_continue.upper_bound(it_ctx->id);
    while (bc_itL != bc_itH)
    {
#ifdef _COMPEL_DEBUG_SCOPE    
      sprintf(temp, "%s%s id=%02d [%02d-%02d] level=%d", ident, s_cmd_continue, it_ctx->id, it_ctx->begin+1, it_ctx->end+1, it_ctx->level);      
#else
      sprintf(temp, "%s %d %d", s_cmd_continue, it_ctx->id, bc_itL->second.level);
#endif
      lines.line(bc_itL->second.cmd_line) = temp;
      ++bc_itL;
    }

    // Found patches for break?
    bc_itL = patch_break.lower_bound(it_ctx->id);
    bc_itH = patch_break.upper_bound(it_ctx->id);

    while (bc_itL != bc_itH)
    {
#ifdef _COMPEL_DEBUG_SCOPE    
      sprintf(temp, "%s%s id=%02d [%02d-%02d] level=%d", ident, s_cmd_break, it_ctx->id, it_ctx->begin+1, it_ctx->end+1, it_ctx->level);      
#else
      sprintf(temp, "%s %d %d", s_cmd_break, it_ctx->id, bc_itL->second.level);
#endif
      lines.line(bc_itL->second.cmd_line) = temp;
      ++bc_itL;
    }

    // Found patches for RETURN?
    bc_itL = patch_return.lower_bound(it_ctx->id);
    bc_itH = patch_return.upper_bound(it_ctx->id);
    while (bc_itL != bc_itH)
    {
#ifdef _COMPEL_DEBUG_SCOPE    
      sprintf(temp, "%s%s id=%02d [%02d-%02d] level=%d", ident, s_cmd_return, it_ctx->id, it_ctx->begin+1, it_ctx->end+1, it_ctx->level);      
#else
      sprintf(temp, "%s %d %s", s_cmd_return, it_ctx->id, bc_itL->second.misc.c_str());      
#endif
      lines.line(bc_itL->second.cmd_line) = temp;
      ++bc_itL;
    }

#ifdef _COMPEL_DEBUG_SCOPE    
    sprintf(temp, "%s%s id=%02d [%02d-%02d] level=%d", ident, s_cmd_scope_begin, it_ctx->id, it_ctx->begin+1, it_ctx->end+1, it_ctx->level);
#else
    sprintf(temp, "%s %d", s_cmd_scope_begin, it_ctx->id);
#endif
    lines.line(it_ctx->begin) = temp;
#ifdef _COMPEL_DEBUG_SCOPE    
    sprintf(temp, "%s%s id=%02d [%02d-%02d] level=%d", ident, s_cmd_scope_end, it_ctx->id, it_ctx->begin+1, it_ctx->end+1, it_ctx->level);
#else
    sprintf(temp, "%s %d", s_cmd_scope_end, it_ctx->id);
#endif
    lines.line(it_ctx->end) = temp;
  }

  // patch FUNCTION
  for (ufnc_context_list_t::iterator it_ufnc = patch_ufnc.begin(); it_ufnc != patch_ufnc.end(); ++it_ufnc)
  {
    // save this function context in the list
    preparse.commands.push_back(*it_ufnc);
#ifdef _COMPEL_DEBUG_SCOPE    
    char *ident = identstr(it_ufnc->level-1);
    sprintf(temp, "%s%s %s %s scopeid=%02d level=%d", 
      ident, 
      s_cmd_command,
      it_ufnc->fnc_name.c_str(),
      it_ufnc->fnc_param.c_str(),
      it_ufnc->scope_id, 
      it_ufnc->level);
#else
    sprintf(temp, "%s %d %s %s", 
      s_cmd_command, 
      it_ufnc->scope_id, 
      it_ufnc->fnc_name.c_str(), 
      it_ufnc->fnc_param.c_str());
#endif
    lines.line(it_ufnc->fnc_line) = temp;
  }

  // patch FOR
  for (for_context_list_t::iterator it_for = patch_for.begin(); it_for != patch_for.end(); ++it_for)
  {
#ifdef _COMPEL_DEBUG_SCOPE    
    char *ident = identstr(it_for->level-1);
    sprintf(temp, "%s%s %s %s %s %s scopeid=%02d level=%d", 
      ident, 
      s_cmd_for,
      it_for->control_var .c_str(),
      it_for->bound1.c_str(),
      it_for->direction.c_str(),
      it_for->bound2.c_str(),
      it_for->scope_id, 
      it_for->level);
#else
    sprintf(temp, "%s %d %s %s %s %s", 
      s_cmd_for,
      it_for->scope_id,
      it_for->control_var .c_str(),
      it_for->bound1.c_str(),
      it_for->direction.c_str(),
      it_for->bound2.c_str());
#endif
    lines.line(it_for->for_line) = temp;
  }

  // patch IF/ELSE
  for (if_context_list_t::iterator it_if = patch_if.begin(); it_if != patch_if.end(); ++it_if)
  {
#ifdef _COMPEL_DEBUG_SCOPE    
    char *ident = identstr(it_if->level-1);
    sprintf(temp, "%s%s %s %s %s t_scopeid=%02d f_scopeid=%02d [%02d-%02d %02d-%02d] level=%d", 
      ident, 
      s_cmd_if,
      it_if->opr1.c_str(),
      it_if->op.c_str(),
      it_if->opr2.c_str(),
      it_if->scope_id1, 
      it_if->scope_id2,
      it_if->begin1+1, it_if->end1+1, 
      it_if->begin2+1, it_if->end2+1, 
      it_if->level);
#else
    sprintf(temp, "%s %d %d %s %s %s", 
      s_cmd_if, 
      it_if->scope_id1, 
      it_if->scope_id2,
      it_if->opr1.c_str(),
      it_if->op.c_str(),
      it_if->opr2.c_str());
#endif
    lines.line(it_if->if_line) = temp;

    if (it_if->else_line != 0)
    {
#ifdef _COMPEL_DEBUG_SCOPE
      sprintf(temp, "%selse scopeid=%02d [%02d-%02d %02d-%02d] level=%d", 
        ident, 
        it_if->scope_id2, 
        it_if->begin1+1, it_if->end1+1, 
        it_if->begin2+1, it_if->end2+1, 
        it_if->level);
#else
      sprintf(temp, "%s %d", s_cmd_else, it_if->scope_id2);
#endif
      lines.line(it_if->else_line) = temp;
    }
  }

#ifdef _COMPEL_DEBUG_SCOPE    
  printf("------\n");
  for (size_t i=0;i<nlines;i++)
  {
    std::string &line = lines.line(i);
    printf("@%0.3d: %s\n", i+1, line.c_str());
  }

  printf("if_stack=%d\n", stk_if.size());
  printf("scope_stack=%d\n", stk_scope.size());
#endif
  return 0;
}

#ifndef _COMPEL_DEBUG_SCOPE

const char *fnc_scopes::fnc_name = "scopes";

parse_errors_e fnc_scopes::prepare(size_t passno, int &start_or_failing_line)
{
  if (passno != cc_nbpasses)
    return parse_error_none;

  interpreter_t *_int = get_interpreter();

  interpreter_helper_t inthlp(_int);

  lines_t &lines_class = inthlp.get_interpreter_lines();

	_preparse.start_or_failing_line = start_or_failing_line;

	int pperr = preparse_scopes(lines_class, _preparse);

  start_or_failing_line = _preparse.start_or_failing_line;

  switch (pperr)
  {
  case IMBALANCE:
    return parse_error_imbalance;
  case WRONGCONTEXT:
  case SCOPENEEDED:
    return parse_error_wrong_context;
  case PARAM_LESS:
    return parse_error_less_param;
  case PARAM_MORE:
    return parse_error_more_param;
  }
  
  fnc_usercommand user_command(_int);

  parse_errors_e err = user_command.register_functions(_preparse.commands);

  return err;
}

parse_errors_e fnc_scopes::register_function(interpreter_t *interpreter)
{
  // we need to mark only one function as 3-PASS
  // since it is a family of functions, it is enough that one function
  // is triggered till we prepare everything
  fnc_scopes *f_scope_begin = new fnc_scopes(interpreter, sc_begin);
  f_scope_begin->set_flags(f_scope_begin->get_flags() | function_flag_pass3);

  fnc_scopes *functions[] =
  {
    f_scope_begin,
    new fnc_scopes(interpreter, sc_end),
    new fnc_scopes(interpreter, sc_break),
    new fnc_scopes(interpreter, sc_continue),
    new fnc_scopes(interpreter, sc_command),
    new fnc_scopes(interpreter, sc_if),
    new fnc_scopes(interpreter, sc_else),
    new fnc_scopes(interpreter, sc_return),
    new fnc_scopes(interpreter, sc_for),
    new fnc_scopes(interpreter, sc_while),
    new fnc_scopes(interpreter, sc_dowhile)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}

parse_errors_e fnc_scopes::execute()
{
  switch(_op)
  {
  case sc_begin:
    return execute_begin();
  case sc_end:
    return execute_end();
  case sc_break:
    return execute_break();
  case sc_else:
    return execute_else();
  case sc_return:
    return execute_return();
  case sc_continue:
    return execute_continue();
  case sc_for:
    return execute_for();
  case sc_while:
    return execute_while();
  case sc_dowhile:
    return execute_dowhile();
  case sc_if:
    return execute_if();
  case sc_command:
    return execute_command();
  }
  return parse_stop_parsing;
}

fnc_scopes::fnc_scopes()
{
}

fnc_scopes::fnc_scopes(const fnc_scopes &rhs)
{
}

void fnc_scopes::operator=(const fnc_scopes &rhs)
{
}

fnc_scopes::~fnc_scopes()
{
  for (scope_context_map_t::iterator it = _scopes_map.begin(); _scopes_map.end() != it;++it)
  {
    scope_context_t &ctx = it->second;
    delete ctx.binding;
  }
  _scopes_map.clear();
}

fnc_scopes::fnc_scopes(interpreter_t *interpreter, sc_operation op)
{
  _op = op;
  switch(_op)
  {
  case sc_begin:
    set_namedesc(s_cmd_scope_begin, "{ SCOPEID");
    set_minmaxargs(1, 1);
    break;
  case sc_end:
    set_namedesc(s_cmd_scope_end, "} SCOPEID");
    set_minmaxargs(1, 1);
    break;
  case sc_break:
    set_namedesc(s_cmd_break, "break SCOPEID [DELTA]");
    set_minmaxargs(1, 2);
    break;
  case sc_else:
    set_namedesc(s_cmd_else, "else SCOPEID");
    set_minmaxargs(1, 1);
    break;
  case sc_return:
    set_namedesc(s_cmd_return, "return SCOPEID [RETVAL]");
    set_minmaxargs(1, 0);
    break;
  case sc_continue:
    set_namedesc(s_cmd_continue, "continue SCOPEID DELTA");
    set_minmaxargs(1, 2);
    break;
  case sc_for:
    set_namedesc(s_cmd_for, "for SCOPEID VAR BEGIN [TO|DOWNTO] END");
    set_minmaxargs(5, 5);
    break;
  case sc_while:
    set_namedesc(s_cmd_while, "while SCOPEID OPR1 OP OPR2");
    set_minmaxargs(4, 4);
    break;
  case sc_dowhile:
    set_namedesc(s_cmd_dowhile, "dowhile SCOPEID OPR1 OP OPR2");
    set_minmaxargs(4, 4);
    break;
  case sc_if:
    set_namedesc(s_cmd_if, "if SCOPEID_TRUE SCOPEID_FALSE A op B");
    set_minmaxargs(5, 5);
    break;
  case sc_command:
    set_namedesc(s_cmd_command, "command SCOPEID fnc_name par1 par2 ... parN");
    set_minmaxargs(1, 0);
    break;
  }
  set_interpreter(interpreter);
}

scope_context_t *fnc_scopes::find_scope(size_t id)
{
  scope_context_map_t::iterator it = _scopes_map.find(id);
  if (_scopes_map.end() == it)
    return 0;
  else
    return &it->second;
}

parse_errors_e fnc_scopes::execute_begin()
{
  interpreter_t *_int = get_interpreter();
  long id = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEID));

  scope_context_t *ctx = find_scope(id);
  if (ctx == 0)
    return parse_error_wrong_context;

  // no binding
  if (ctx->binding == 0)
    return parse_error_none;

  scope_binding_t *binding = ctx->binding;

  // FUNCTION - binding
  if (binding->bind_type == OCCUPY_FNC)
  {
    return fnc_usercommand::enter_function(_int);
  }
  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_end()
{
  interpreter_t *_int = get_interpreter();
  long id = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEID));

  scope_context_t *ctx = find_scope(id);
  if (ctx == 0)
    return parse_error_wrong_context;

  if (ctx->binding == 0)
    return parse_error_none;

  // handle IF binding
  else if (ctx->binding->bind_type == OCCUPY_IF)
  {
    // IF binding will be created from 'if' only
    // if the condition failed
    scope_if_binding_t *if_binding = static_cast<scope_if_binding_t *>(ctx->binding);
    _int->set_cur_src_line(if_binding->at_after_condition);

    // clear binding
    delete if_binding;

    ctx->binding = 0;
    return parse_branch_to;
  }
  //
  // FOR - binding
  //
  else if (ctx->binding->bind_type == OCCUPY_FOR)
  {
    scope_for_binding_t *for_binding = static_cast<scope_for_binding_t *>(ctx->binding);

    // Get the control variable
    if ( (for_binding->ctrl_var = _int->get_value(for_binding->ctrl_var_name.c_str())) == 0)
      return parse_error_wrong_context;

    long varval = for_binding->ctrl_var->get_int_value();

    bool bDone = false;

    do
    {
      // BREAK instruction encountered?
      if (for_binding->broken)
      {
        bDone = true;
        break;
      }

      // advance?
      if (for_binding->direction > 0)
      {
        ++varval;
        if (varval  > for_binding->bound2)
          bDone = true;
        for_binding->ctrl_var->set_int_value(varval);
      }
      else
      {
        // recede
        --varval;
        if (varval < for_binding->bound2)
          bDone = true;

        for_binding->ctrl_var->set_int_value(varval);
      }
    } while(0);

    if (bDone)
    {
      // clear binding, we can recycle later
      delete for_binding;
      ctx->binding = 0;
    }
    else
    {
      // loop again
      _int->set_cur_src_line(ctx->begin);
      return parse_branch_to;
    }
  }
  else if (ctx->binding->bind_type == OCCUPY_FNC)
  {
    return fnc_usercommand::leave_function(_int);
  }
  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_continue()
{
  interpreter_t *_int = get_interpreter();
  long id = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEID));

  static const int PAR_SCOPEDELTA = 2;
  if (_int->get_fnc_arg_count()-1 == PAR_SCOPEDELTA)
    id += -compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEDELTA));

  scope_context_t *ctx = find_scope(id);
  if (ctx == 0)
    return parse_error_wrong_context;

  // works with all bindings
  if (ctx->binding == 0)
  {
    _int->set_cur_src_line(ctx->begin);
    return parse_branch_to;
  }

  // for-bindng
  if (ctx->binding->bind_type == OCCUPY_FOR)
  {
    scope_for_binding_t *for_binding = static_cast<scope_for_binding_t *>(ctx->binding);
    _int->set_cur_src_line(ctx->end);
    return parse_branch_to;
  }

  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_break()
{
  interpreter_t *_int = get_interpreter();
  long id = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEID));

  static const int PAR_SCOPEDELTA = 2;
  if (_int->get_fnc_arg_count()-1 == PAR_SCOPEDELTA)
    id += -compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEDELTA));

  scope_context_t *ctx = find_scope(id);
  if (ctx == 0)
    return parse_error_wrong_context;

  // no binding
  if (ctx->binding == 0)
  {
    _int->set_cur_src_line(ctx->end);
    return parse_branch_to;
  }

  bool bBranch = false;

  // for-binding
  if (ctx->binding->bind_type == OCCUPY_FOR)
  {
    scope_for_binding_t *for_binding = static_cast<scope_for_binding_t *>(ctx->binding);
    for_binding->broken = true;
    bBranch = true;
  }

  if (bBranch)
  {
    _int->set_cur_src_line(ctx->end);
    return parse_branch_to;
  }

  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_return()
{
  return fnc_usercommand::leave_function(get_interpreter(), true);
}

parse_errors_e fnc_scopes::execute_if()
{
  static const int PAR_TRUE_SCOPE   = 1;
  static const int PAR_FALSE_SCOPE  = 2;
  static const int PAR_OPR1         = 3;
  static const int PAR_OP           = 4;
  static const int PAR_OPR2         = 5;

  interpreter_t *_int = get_interpreter();

  // get the operator's type
  int op = fnc_binary_comparison::oprstr_to_opr(_int->get_const_at(PAR_OP));
  if (op == -1)
  {
    return parse_error_wrong_syntax;
  }

  // do the checks
  bool bCondition = fnc_binary_comparison::do_comparison(_int, op, PAR_OPR1, PAR_OPR2);

  long true_scope  = compel_string_tokenizer_t::parse_number(_int->get_const_at(PAR_TRUE_SCOPE));
  long false_scope = compel_string_tokenizer_t::parse_number(_int->get_const_at(PAR_FALSE_SCOPE));

  scope_context_t *true_ctx  = find_scope(true_scope);
  scope_context_t *false_ctx = find_scope(false_scope);

  // we have no ELSE
  // and TRUE is set. Usually true scope must always be there
  // but it might not be present in case of syntax error
  // since we don't really enforce syntax checks
  if ((false_scope == 0) && (true_ctx != 0))
  {
    if (bCondition == false)
      _int->set_cur_src_line(true_ctx->end);
    else
      _int->set_cur_src_line(true_ctx->begin);
  }
  // false scope
  else if (false_ctx != 0)
  {
    // should execute the ELSE?
    if (bCondition == false)
    {
      _int->set_cur_src_line(false_ctx->begin);
    }
    else
    {
      scope_if_binding_t *if_binding = new scope_if_binding_t;
      if_binding->at_after_condition = false_ctx->end;
      true_ctx->binding = if_binding;
      _int->set_cur_src_line(true_ctx->begin);
    }
  }
  else
    return parse_error_wrong_context;

  return parse_branch_to;
}

parse_errors_e fnc_scopes::execute_else()
{
  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_for()
{
  static const int PAR_SCOPE   = 1;
  static const int PAR_VAR     = 2;
  static const int PAR_BOUND1  = 3;
  static const int PAR_DIR     = 4;
  static const int PAR_BOUND2  = 5;

  interpreter_t *_int = get_interpreter();

  // Determine direction
  const char *str_dir = _int->get_const_at(PAR_DIR);
  long  ndir;
  if (stricmp(str_dir, s_for_dir_fwd) == 0)
    ndir = 1;
  else if (stricmp(str_dir, s_for_dir_prev) == 0)
    ndir = -1;
  else
  {
    _int->set_msg_error(s_for_err_nodir);
    return parse_error_wrong_syntax;
  }

  // Find the scope
  scope_context_t *ctx = find_scope(
    compel_string_tokenizer_t::parse_number(_int->get_const_at(PAR_SCOPE)));

  if (ctx == 0)
    return parse_error_wrong_context;

  // get variable's name
  const char *varname = _int->get_const_at(PAR_VAR);

  // is this a variable?
  if (!_int->is_variable_name(varname))
    return parse_error_variable_expected;

  // Get the counting variable
  value_t *var = _int->get_value_at(PAR_VAR);
  if (var == 0)
    return parse_error_symbol_not_in_table;

  scope_for_binding_t *for_binding = new scope_for_binding_t;

  // store the control variable name
  for_binding->ctrl_var_name = varname;

  // Assign context binding
  ctx->binding = for_binding;

  // set bound1 and bound2
  for_binding->bound1 = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_BOUND1));
  for_binding->bound2 = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_BOUND2));

  // save control variable
  for_binding->ctrl_var = var;

  // save direction
  for_binding->direction = ndir;

  // Initialize control variable
  var->set_int_value(for_binding->bound1);

  // go to scope's beginning
  _int->set_cur_src_line(ctx->begin);

  return parse_branch_to;
}

parse_errors_e fnc_scopes::execute_while()
{
  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_dowhile()
{
  return parse_error_none;
}

parse_errors_e fnc_scopes::execute_command()
{
  interpreter_t *_int = get_interpreter();
  long id = compel_string_tokenizer_t::parse_number(_int->evaluate_at(PAR_SCOPEID));

  scope_context_t *ctx = find_scope(id);
  if (ctx == 0)
    return parse_error_wrong_context;

  // 1. should create the function's definition
  // 2. and register it in the symbol table

  // 3. should skip its scope and execute next statement
  _int->set_cur_src_line(ctx->end + 1);

  return parse_branch_to;
}

#endif
