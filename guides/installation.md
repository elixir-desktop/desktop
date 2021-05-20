# Getting Started with Elixir Desktop

## General notes

Elixir Desktop depends on the *bleeding edge* versions of 

* Erlang OTP/24
* Elixir 1.12
* WxWidgets 3.1.15

Unfortunately these versions are not yet (2021-05-20) available on most OS package managers and such manual installation along these instructions is needed:

## Building on Ubuntu Linux

**Install System Dependencies:**

```bash
sudo apt install inotify-tools libtool automake libgmp-dev make libwxgtk-webview3.0-gtk3-dev libssl-dev libncurses5-dev curl git
```

**Install wxWidgets 3.1.5:**

```bash
sudo apt install libjpeg-dev libpng-dev libtiff-dev zlib1g-dev libncurses5-dev libssh-dev unixodbc-dev libgmp3-dev libwxbase3.0-dev libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libsctp-dev lksctp-tools build-essential libgtk-3-dev libnotify-dev libsecret-1-dev catch

mkdir ~/projects && cd ~/projects
git clone https://github.com/wxWidgets/wxWidgets.git
cd wxWidgets; 
git checkout v3.1.5
./configure --prefix=/usr/local/wxWidgets --enable-clipboard --enable-controls \
      --enable-dataviewctrl --enable-display \
      --enable-dnd --enable-graphics_ctx \
      --enable-std_string --enable-svg \
      --enable-unicode --enable-webview \
      --with-expat --with-libjpeg \
      --with-libpng --with-libtiff \
      --with-opengl --with-zlib \
      --disable-precomp-headers --disable-monolithic

make -j4
```


**Install Erlang OTP/24:**

```bash
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod a+x kerl
sudo mv kerl /usr/bin/
export LD_LIBRARY_PATH=$HOME/projects/wxWidgets/lib
echo "export LD_LIBRARY_PATH=\"$HOME/projects/wxWidgets/lib\"" >> ~/.bashrc
export KERL_CONFIGURE_OPTIONS=--with-wxdir=$HOME/projects/wxWidgets
kerl build git https://github.com/erlang/otp.git maint-24 maint-24
kerl install maint-24 ~/maint-24
. ~/maint-24/activate
```

**Install Elixir 1.12:**

```bash
mkdir $HOME/elixir && cd $HOME/elixir
wget https://github.com/elixir-lang/elixir/releases/download/v1.12/Precompiled.zip
unzip Precompiled.zip
echo "export PATH=\"$HOME/elixir/bin:\$PATH\"" >> ~/.bashrc
export PATH="$HOME/elixir/bin:$PATH"
```

**Install NodeJS:**

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
nvm install v12.16.1
```
*Note: If you get "nvm: command not found" after running "nvm install v12.16.1" in the terminal, try closing the current teminal, opening a new one, and then running "nvm install v12.16.1" again. (see https://github.com/nvm-sh/nvm/blob/master/README.md#troubleshooting-on-linux for more information).*


**Troubleshooting non-displayed taskbar icons on Ubuntu**

Install [topicons](https://extensions.gnome.org/extension/495/topicons/) or [topiconsfix](https://extensions.gnome.org/extension/1674/topiconsfix) GNOME shell extension to access the dDrive GUI.


## Building on macOS

These instructions are based on `brew`.

**System dependencies:**

```bash
brew install binutils automake elixir kerl libtool gmp
export PATH=/usr/local/opt/binutils/bin:$PATH
```

WxWidgets with recent macOS fixes

```bash
brew tap desktop/homebrew-extra
brew install desktop/extra/wxmac@3.1.5 --HEAD
brew link desktop/extra/wxmac@3.1.5
```

**Install Erlang OTP/24**

```bash
kerl build git https://github.com/erlang/otp.git maint-24 maint-24
kerl install maint-24 ~/maint-24
. ~/maint-24/activate
```

**Install nodejs / npm 12.16.1**

```bash
brew install nvm
nvm install v12.16.1
```

## Building on Windows

Working and compiling under Windows is probably the most difficult. The setup is using [msys2](https://www.msys2.org/) instead of WSL because we want to produce a native Desktop application that can run on any Windows machine even without WSL installed...

**Install Dependencies:**

* Install Erlang 24: https://erlang.org/download/otp_win64_24.0.exe
* Install msys2 https://www.msys2.org/ 

Install additional msys2 packages from an msys2 64-bit shell

```bash
pacman -Syu
pacman -S --noconfirm pacman-mirrors pkg-config
pacman -S --noconfirm --needed base-devel autoconf automake make libtool mingw-w64-x86_64-toolchain mingw-w64-x86_64-openssl mingw-w64-x86_64-libtool git
```

**Install Elixir 1.12:**

From an msys2 64-bit shell do:

```bash
mkdir $HOME/elixir && cd $HOME/elixir
wget https://github.com/elixir-lang/elixir/releases/download/v1.12/Precompiled.zip
unzip Precompiled.zip
echo "export PATH=\"$HOME/elixir/bin:\$PATH\"" >> ~/.bashrc
export PATH="$HOME/elixir/bin:$PATH"
```

**Install Node ***

Get npm & node 12.x from https://nodejs.org/dist/latest-v12.x/

Then in msys2 64-bit shell add the nodejs binary path to your `PATH` variable.


**Install dependencies to build an installer**

Get nsis https://nsis.sourceforge.io/Main_Page
