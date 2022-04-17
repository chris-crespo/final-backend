.PHONY: run setup

run:
	@awful run.scm --development-mode	

setup: 
	@export DATABASE_URL=$(shell heroku config | grep DATABASE_URL | awk '{ print $$2 }')
	@awful run.scm --development-mode	
