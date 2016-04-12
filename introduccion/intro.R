remove                  package:base                   R Documentation

_R_e_m_o_v_e _O_b_j_e_c_t_s _f_r_o_m _a _S_p_e_c_i_f_i_e_d _E_n_v_i_r_o_n_m_e_n_t

_D_e_s_c_r_i_p_t_i_o_n:

     ‘remove’ and ‘rm’ can be used to remove objects.  These can be
     specified successively as character strings, or in the character
     vector ‘list’, or through a combination of both.  All objects thus
     specified will be removed.

     If ‘envir’ is NULL then the currently active environment is
     searched first.

     If ‘inherits’ is ‘TRUE’ then parents of the supplied directory are
     searched until a variable with the given name is encountered.  A
     warning is printed for each variable that is not found.

_U_s_a_g_e:

     remove(..., list = character(), pos = -1,
            envir = as.environment(pos), inherits = FALSE)
     
     rm    (..., list = character(), pos = -1,
            envir = as.environment(pos), inherits = FALSE)
     
_A_r_g_u_m_e_n_t_s:

     ...: the objects to be removed, as names (unquoted) or character
          strings (quoted).

    list: a character vector naming objects to be removed.

     pos: where to do the removal.  By default, uses the current
          environment.  See ‘details’ for other possibilities.

   envir: the ‘environment’ to use.  See ‘details’.

inherits: should the enclosing frames of the environment be inspected?

_D_e_t_a_i_l_s:

     The ‘pos’ argument can specify the environment from which to
     remove the objects in any of several ways: as an integer (the
     position in the ‘search’ list); as the character string name of an
     element in the search list; or as an ‘environment’ (including
     using ‘sys.frame’ to access the currently active function calls).
     The ‘envir’ argument is an alternative way to specify an
     environment, but is primarily there for back compatibility.

     It is not allowed to remove variables from the base environment
     and base namespace, nor from any environment which is locked (see
     ‘lockEnvironment’).

     Earlier versions of R incorrectly claimed that supplying a
     character vector in ‘...’ removed the objects named in the
     character vector, but it removed the character vector.  Use the
     ‘list’ argument to specify objects _via_ a character vector.

_R_e_f_e_r_e_n_c_e_s:

     Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) _The New S
     Language_.  Wadsworth & Brooks/Cole.

_S_e_e _A_l_s_o:

     ‘ls’, ‘objects’

_E_x_a_m_p_l_e_s:

     tmp <- 1:4
     ## work with tmp  and cleanup
     rm(tmp)
     
     ## Not run:
     
     ## remove (almost) everything in the working environment.
     ## You will get no warning, so don't do this unless you are really sure.
     rm(list = ls())
     ## End(Not run)
     

