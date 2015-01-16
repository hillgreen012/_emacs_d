install_xinix () {
    printf " Cloning the Xinix's GitHub repository...\n$RESET"
    if [ x$XINIX_VERBOSE != x ]
    then
        /usr/bin/env git clone $XINIX_URL "$XINIX_INSTALL_DIR"
    else
        /usr/bin/env git clone $XINIX_URL "$XINIX_INSTALL_DIR" > /dev/null
    fi
    if ! [ $? -eq 0 ]
    then
        printf "$RED A fatal error occurred during Xinix's installation. Aborting..."
        exit 1
    fi
}

make_xinix_dirs () {
    printf " Making the required directories.\n$RESET"
    mkdir -p "$XINIX_INSTALL_DIR/savefile"
}

colors_ () {
    case "$SHELL" in
    *zsh)
        autoload colors && colors
        eval RESET='$reset_color'
        for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE
        do
            eval $COLOR='$fg_no_bold[${(L)COLOR}]'
            eval B$COLOR='$fg_bold[${(L)COLOR}]'
        done
        ;;
    *)
        RESET='\e[0m'           # Reset
        RED='\e[0;31m'          # Red
        GREEN='\e[0;32m'        # Green
        YELLOW='\e[0;33m'       # Yellow
        BLUE='\e[0;34m'         # Blue
        PURPLE='\e[0;35m'       # Magenta
        CYAN='\e[0;36m'         # Cyan
        WHITE='\e[0;37m'        # White
        BRED='\e[1;31m'         # Bold Red
        BGREEN='\e[1;32m'       # Bold Green
        BYELLOW='\e[1;33m'      # Bold Yellow
        BBLUE='\e[1;34m'        # Bold Blue
        BPURPLE='\e[1;35m'      # Bold Magenta
        BCYAN='\e[1;36m'        # Bold Cyan
        BWHITE='\e[1;37m'       # Bold White
        ;;
    esac
}

# Commandline args:
# -d/--directory [dir]
#   Install xinix into the specified directory. If 'dir' is a relative path prefix it with $HOME.
#   Defaults to '$HOME/.emacs.d'
# -c/--colors
#   Enable colors
# -s/--source [url]
#   Clone xinix from 'url'.
#   Defaults to 'https://github.com/bbatsov/xinix.git'
# -i/--into
#   If one exists, install into the existing config
# -n/--no-bytecompile
#   Skip the compilation of the xinix files.
# -h/--help
#   Print help
# -v/--verbose
#   Verbose output, for debugging

usage() {
    printf "Usage: $0 [OPTION]\n"
    printf "  -c, --colors \t \t \t Enable colors.\n"
    printf "  -d, --directory [dir] \t Install xinix into the specified directory.\n"
    printf "  \t \t \t \t If 'dir' is a relative path prefix with $HOME.\n"
    printf "  \t \t \t \t Defaults to $HOME/.emacs.d\n"
    printf "  -s, --source [url] \t \t Clone xinix from 'url'.\n"
    printf "  \t \t \t \t Defaults to 'https://github.com/bbatsov/xinix.git'.\n"
    printf "  -n, --no-bytecompile \t \t Skip the bytecompilation step of xinix.\n"
    printf "  -i, --into \t \t \t Install Xinix into a subdirectory in the existing configuration\n"
    printf "  \t \t \t \t The default behavious is to install xinix into the existing\n"
    printf "  \t \t \t \t emacs configuration.\n"
    printf "  -h, --help \t \t \t Display this help and exit\n"
    printf "  -v, --verbose \t \t Display verbose information\n"
    printf "\n"
}

### Parse cli
while [ $# -gt 0 ]
do
    case $1 in
        -d | --directory)
            XINIX_INSTALL_DIR=$2
            shift 2
            ;;
        -c | --colors)
            colors_
            shift 1
            ;;
        -s | --source)
            XINIX_URL=$2
            shift 2
            ;;
        -i | --into)
            XINIX_INTO='true'
            shift 1
            ;;
        -n | --no-bytecompile)
            XINIX_SKIP_BC='true'
            shift 1
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        -v | --verbose)
            XINIX_VERBOSE='true';
            shift 1
            ;;
        *)
            printf "Unkown option: $1\n"
            shift 1
            ;;
    esac
done

VERBOSE_COLOR=$BBLUE

[ -z "$XINIX_URL" ] && XINIX_URL="https://github.com/bbatsov/xinix.git"
[ -z "$XINIX_INSTALL_DIR" ] && XINIX_INSTALL_DIR="$HOME/.emacs.d"

if [ x$XINIX_VERBOSE != x ]
then
    printf "$VERBOSE_COLOR"
    printf "XINIX_VERBOSE = $XINIX_VERBOSE\n"
    printf "INSTALL_DIR = $XINIX_INSTALL_DIR\n"
    printf "SOURCE_URL  = $XINIX_URL\n"
    printf "$RESET"
    if [ -n "$XINIX_SKIP_BC" ]
    then
        printf "Skipping bytecompilation.\n"
    fi
    if [ -n "$XINIX_INTO" ]
    then
        printf "Replacing existing config (if one exists).\n"
    fi
    printf "$RESET"
fi

# If xinix is already installed
if [ -f "$XINIX_INSTALL_DIR/core/xinix-core.el" ]
then
    printf "\n\n$BRED"
    printf "You already have Xinix installed.$RESET\nYou'll need to remove $XINIX_INSTALL_DIR/xinix if you want to install Xinix again.\n"
    printf "If you want to update your copy of xinix, run 'git pull origin master' from your xinix directory\n\n"
    exit 1;
fi

### Check dependencies
printf  "$CYAN Checking to see if git is installed... $RESET"
if hash git 2>&-
then
    printf "$GREEN found.$RESET\n"
else
    printf "$RED not found. Aborting installation!$RESET\n"
    exit 1
fi;

printf  "$CYAN Checking to see if aspell is installed... "
if hash aspell 2>&-
then
    printf "$GREEN found.$RESET\n"
else
    printf "$RED not found. Install aspell to benefit from flyspell-mode!$RESET\n"
fi

### Check emacs version
if [ $(emacs --version 2>/dev/null | sed -n 's/.*[^0-9.]\([0-9]*\.[0-9.]*\).*/\1/p;q' | sed 's/\..*//g') -lt 24 ]
then
    printf "$YELLOW WARNING:$RESET Xinix depends on emacs $RED 24$RESET !\n"
fi

if [ -d "$XINIX_INSTALL_DIR" ] || [ -f "$XINIX_INSTALL_DIR" ]
then
    # Existing file/directory found -> backup
    printf " Backing up the existing config to $XINIX_INSTALL_DIR.pre-xinix.tar.\n"
    tar -cf "$XINIX_INSTALL_DIR.pre-xinix.tar" "$XINIX_INSTALL_DIR" > /dev/null 2>&1
    XINIX_INSTALL_DIR_ORIG="$XINIX_INSTALL_DIR"
    # Overwrite existing?
    [ -n "$XINIX_INTO" ] && XINIX_INSTALL_DIR="$XINIX_INSTALL_DIR/xinix"
    # Clear destination directory for git clone to work
    rm -fr "$XINIX_INSTALL_DIR"
    mkdir "$XINIX_INSTALL_DIR"
    # Replace existing config
    install_xinix
    make_xinix_dirs
    # Reinstate files that weren't replaced
    tar --skip-old-files -xf "$XINIX_INSTALL_DIR_ORIG.pre-xinix.tar" "$XINIX_INSTALL_DIR" > /dev/null 2>&1
    [ -n "$XINIX_INTO" ] && cp "$XINIX_INSTALL_DIR/sample/xinix-modules.el" "$XINIX_INSTALL_DIR"
elif [ -e "$XINIX_INSTALL_DIR" ]
then
    # File exist but not a regular file or directory
    # WTF NOW?
    printf "$BRED $XINIX_INSTALL_DIR exist but isn't a file or directory.\n"
    printf "$BRED please remove this file or install xinix in a different directory"
    printf "$BRED (-d flag)\n$RESET"
    exit 1
else
    # Nothing yet so just install xinix
    install_xinix
    make_xinix_dirs
    cp "$XINIX_INSTALL_DIR/sample/xinix-modules.el" "$XINIX_INSTALL_DIR"
fi

if [ -z "$XINIX_SKIP_BC" ];
then
    if which emacs > /dev/null 2>&1
    then
        printf " Bytecompiling Xinix.\n"
        if [ x$XINIX_VERBOSE != x ]
        then
            emacs -batch -f batch-byte-compile "$XINIX_INSTALL_DIR/core"/*.el
        else
            emacs -batch -f batch-byte-compile "$XINIX_INSTALL_DIR/core"/*.el > /dev/null 2>&1
        fi
    else
        printf "$YELLOW Emacs not found.$RESET Skipping bytecompilation.\n"
    fi
else
    printf "Skipping bytecompilation.\n"
fi

printf "\n"
printf "$BBLUE  ____           _           _       \n"
printf "$BBLUE |  _ \ _ __ ___| |_   _  __| | ___  \n"
printf "$BBLUE | |_) |  __/ _ \ | | | |/ _  |/ _ \ \n"
printf "$BBLUE |  __/| | |  __/ | |_| | (_| |  __/ \n"
printf "$BBLUE |_|   |_|  \___|_|\__,_|\__,_|\___| \n\n"
printf "$GREEN ... is now installed and ready to do thy bidding, Master $USER!$RESET\n"
printf "$GREEN Don't forget to adjust the modules you want to use in $XINIX_INSTALL_DIR/xinix-modules.el!$RESET\n"
