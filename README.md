Emacs configuration
===================

## How to setup ##

1. Clone the repository into folder, chosen by your decision (further called **$CONFIG_DIR$**):

	   ```bash
	$ git clone git://github.com/ajukraine/ajukraine-dotemacs.git
	$ git submodule init
	$ git submodule update
	   ```

2. Create a symlink to *.emacs* file:

	   ```bash
	$ ln -s $CONFIG_DIR$/.emacs ~/.emacs
	   ```

3. Reload *~/.emacs* file
