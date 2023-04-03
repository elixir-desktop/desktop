# Getting your Environment Ready

## General notes

Elixir Desktop depends on recent versions of

* Erlang >= OTP/24 (with wxWidgets >= 3.1.15)
* Elixir >= 1.11.4

## Windows

The setup is using [msys2](https://www.msys2.org/) instead of WSL because we want to produce a native Desktop application that can run on any Windows machine even without WSL installed.

**Install Dependencies:**

* Install Erlang 24: https://erlang.org/downloads
  * Or Build: https://erlang.org/doc/installation_guide/INSTALL-WIN32.html

* Install msys2 https://www.msys2.org/ 

Install additional msys2 packages from an msys2 64-bit shell

```bash
pacman -Syu
pacman -S --noconfirm pacman-mirrors pkg-config
pacman -S --noconfirm --needed base-devel autoconf automake make libtool mingw-w64-x86_64-toolchain mingw-w64-x86_64-openssl mingw-w64-x86_64-libtool git
```

**Install Elixir 1.12 (or higher)**

From an msys2 64-bit shell do:

```bash
mkdir $HOME/elixir && cd $HOME/elixir
wget https://github.com/elixir-lang/elixir/releases/download/v1.12/Precompiled.zip
unzip Precompiled.zip
echo "export PATH=\"$HOME/elixir/bin:\$PATH\"" >> ~/.bashrc
export PATH="$HOME/elixir/bin:$PATH"
```

**Install Node**

We're using esbuild in the new Example app, but you will still need npm at least. 
Get npm & node 12.x (or higher) from https://nodejs.org/dist/latest-v12.x/

Then in msys2 64-bit shell add the nodejs binary path to your `PATH` variable.

**Install dependencies to build an installer**

Get nsis https://nsis.sourceforge.io/Main_Page

## MacOS

Use brew and it will fetch a recent version of Erlang & Elixir:

```bash
brew install elixir
```

For building NIFs and packaging a release you will also need to get Xcode and install the
Xcode Command Line tools. To install the necessary Xcode tools using Xcode on the Mac:

* Start Xcode on the Mac.
* Choose Preferences from the Xcode menu.
* In the General panel, click Downloads.
* On the Downloads window, choose the Components tab.
* Click the Install button next to Command Line Tools.
    - You are asked for your Apple Developer login during the install process. 

**Install nodejs / npm 12.16.1**

```bash
brew install nvm
nvm install v12.16.1
```

**Using custom WxWidgets**

On macOS good wxWidgets support is pretty recent. You might want to try a newer e.g. master version when you spot visual issues in the native Menus or Taskbar Icon behaviour:

```bash
mkdir ~/projects && cd ~/projects
git clone https://github.com/wxWidgets/wxWidgets.git
cd wxWidgets
git checkout master
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

**Build Erlang using custom wxWidgets**

```bash
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod a+x kerl
sudo mv kerl /usr/bin/
export LD_LIBRARY_PATH=$HOME/projects/wxWidgets/lib
export KERL_CONFIGURE_OPTIONS=--with-wxdir=$HOME/projects/wxWidgets
kerl build git https://github.com/erlang/otp.git maint-24 maint-24
kerl install maint-24 ~/maint-24
. ~/maint-24/activate
```

If you're building a custom version of wxWidgets & Erlang you might want to add these to your `.bashrc` or `.zshrc`:

```bash
echo "export LD_LIBRARY_PATH=\"$HOME/projects/wxWidgets/lib\"" >> ~/.bashrc
echo ". ~/maint-24/activate" >> ~/.bashrc

```

## GNU/Linux

Best to use Erlang solutions packages: https://www.erlang-solutions.com/downloads/

**Or use ASDF** 
```
asdf plugin-update --all
asdf install erlang 24.0.1
```

**Install NIF Dependencies:**

For compiling NIFs you will need a c compiler and dependencies, such as from:

```bash
sudo apt install inotify-tools libtool automake libgmp-dev make libwxgtk-webview3.0-gtk3-dev libssl-dev libncurses5-dev curl git
```
