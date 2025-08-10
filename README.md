# .dotfiles

My dotfiles.

The commit history isn't the best, as I started this repository as a private
repo just to sync my dotfiles.

## Installation

Installation requirements: `git` and `stow` (GNU Stow)

```bash
git clone git@github.com:yrjarv/.dotfiles ~/.dotfiles
cd ~/.dotfiles
stow */
```

## "Documentation"/description of everything in the repo

### `.scripts`

Directory name has a dot in front of it so that the `*` in `stow *` doesn't make
`stow` try to stow the scripts.

I previously tried to create a script for after-install setup of my arch
installations, but that failed.

Now, the directory only contains `devilry_colors.js` - a script made by [Johann
Tveranger](https://www.github.com/johtve) to make the [Devilry
](https://devilry.ifi.uio.no) UI a little bit less confusing. I have modified it
slightly, so that there is even less visual clutter. To use the script, install
Tampermonkey in your browser.

### `bash_profile` and `bashrc`

I only keep this around for historical purposes, as I have now (hopefully) fully
made the switch to Zsh. My `.bashrc` file is a mess, but it served me well.

### `clang-format`

Only contains a very simple `.clang-format` file, that `clang` uses when trying
to automatically format my `c` programs.

### `config`

* `.config/btop` and `.config/htop` contain config files for `htop` and `btop`,
  but they are mainly modified through the `btop` and `htop` TUIs. I track them
  with git because misclicks happen, and especially in `btop` I find it really
  hard to reverse any changes I made to the config when I don't have the config
  file version controlled.
* `.config/hypr` is where I have the config file for Hyprland. `hyprland.conf`
  is mostly the automatically generated file that every new install comes with,
  but I have made quite a few changes - especially to the keybinds.
* `.config/kitty` is a very simple config file for kitty, mostly to make it look
  even more minimalistic than it is by default.
* `.config/waybar` contains my _very_ minimalistic Waybar config: It is black
great bash configuration, I haven't really tried to make the switch yet.
  with white text, and absolutely no icons.

### `emacs.d`

I am still trying to figure out if I should use neovim or emacs. In [my `nvim`
repository](https://www.github.com/yrjarv/nvim) I am currently setting up
neovim, but this is where I keep a somewhat functioning emacs config - so I have
something to fall back on if I need to do something important while I configure
neovim.

### `gitconfig`

This can't be copied without changing the email and name (unless you want me to
be a contributor to all your repositories...).

The main purpose of my `.gitconfig` file is to have a few aliases:

* `last -X`, where `X` is the number of commits to show, limits the amount
  of commits shown with `git log`: You might not want all the commits, only e.g.
  the hash of the previous one - in which case you can run `git last -1`.
* `oneline` is the same as `git log --oneline`, but slightly quicker to
  write.
* `aco "MESSAGE"` - my most used alias, it stages all modified files and commits
  them with the message you choose.
* `word-diff`: just like `oneline` it is a shortening of `git diff --word-diff`,
  for no other reason than that it is quicker to write.
* `graph` is one of the most interesting ones. It prints a nice graph of all
  commits on all branches, with better formatting than `git log --graph`.
* `contributors` isn't very much used, byt still quite useful. It lists all the
  contributors (if the email addresses are different, they are separated)
  alongside the number of commits they have made.

Additionally, all commits are should automatically get signed.

### `ssh`

My `.ssh/config` file probably isn't very useful for anyone else, except maybe
a few people at ifi: It is very repetetive, but allows me to run `ssh guanin`
instead of `ssh yrjarv@guanin.uio.no -J yrjarv@morgoth.uio.no` when I want to
acces the department's `guanin` server.

I have set the default jumphost for ifi servers to be `morgoth`, for two
reasons:

1. It is physically located in the department's basement, so it's about 100
   meters away from me - compared to `gothmog`, which is in the city center.
2. When I use `ssh` at home, I need to use 2fa to get into the jumphost. But:
   UiO servers are configured to cache 2fa for a while. So if I concistently use
   the same jumphost server, I can utilize the 2fa caching fully.


### `tmux.conf`

I am trying to learn to use tmux, so this is where I am trying to create a tmux
configuration.

### `zshrc`

I use Oh My Zsh, which can be installed with the following command:

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

The part of `.zshrc` I have made by myself is a bit of a mess, containing a lot
of aliases I use daily.

Because my Kali VM's don't have all the programs I use installed on them, quite
a few aliases are "hidden" behind an if statement, checking if I am on my own
system or a Kali VM.

Additionally, there are some weird quirks with how I deal with Ifi servers: I
have a few binaries in `~/.binaries/bin`, because I can neither install nor
upgrade with `dnf` on them (no sudo permissions), which is why I prepend
`~/.binaries/bin` to PATH.

There are also a few aliases that won't work for anyone else without
modification, such as my `uio-sftp` function - which uses my username to ssh
into the UiO SFTP servers.

I also have a few aliases and functions for simplifying my git workflow: I have
one repository for each of the courses I take, and `cd`-ing between all of them
just to make sure all my changes are pushed would take too long. So I have three
aliases: `push`, `pull`, and `status`, all of which run their respective `git`
commands in all the git repositories directly inside `~`.

An additional "weird" thing I have done with my `.zshrc`: I have a command (`(ll
-L=2 &) > /dev/null`) at the bottom. This is pretty much the same as `tree` -
except with a lot fewer files being loaded. The reason for this is that the file
system on the Ifi servers is very slow (all the files are on one physical
server, which is connected to all the computing servers), so by accessing most
of my files in the background when logging in, the files I am most likely to
need are cached. I have zero proof that that's what happens, but the access
times are dramatically reduced after adding that line - so I assume that it
helps.
