# Start configuration added by Zim install {{{
#
# User configuration sourced by all invocations of the shell
#

# Define Zim location
: ${ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim}
# }}} End configuration added by Zim install
typeset -U PATH path
path=(/usr/local/bin
  /usr/bin
  ~/.local/bin
  ~/.cargo/bin
  /opt/openresty/bin
  /opt/intel/compilers_and_libraries_2019.5.281/linux/bin
  /opt/apache-drill/bin
  /opt/apache-spark/bin
  /home/perrierjouet/istio-1.5.1/bin
  /data/anaconda3/bin
  /home/perrierjouet/go/bin
  /snap/bin/)
export PATH

