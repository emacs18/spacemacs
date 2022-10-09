
REPOS = .local/straight/repos

all : $(REPOS)/straight.el

$(REPOS)/straight.el :
	mkdir -p $(REPOS)
	cd $(REPOS); git clone -b develop --depth 10 https://github.com/radian-software/straight.el.git
