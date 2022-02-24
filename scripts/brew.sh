install_brew() {
    if test "$(uname)" != "Darwin" ; then
      echo Skipping brew as this device is not running macOS
      return
    fi
    brew bundle --file scripts/Brewfile
}

