set -gx LDFLAGS "-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"	
set -gx CPPFLAGS "-I/usr/local/opt/llvm/include"	
# set -gx LDFLAGS "-L/usr/local/opt/llvm/lib"	
set -gx fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
