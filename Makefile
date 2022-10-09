
REPOS = .local/straight/repos

all : bootstrap
	mkdir .spacemacs

bootstrap : $(REPOS)/straight.el

$(REPOS)/straight.el :
	mkdir -p $(REPOS)
	cd $(REPOS); git clone -depth 10 https://github.com/raxod502/straight.el.git
