objects = hello.o world.o example.o

RST2HTML = rst2html.py

helloworld:
	curl -vv --compressed -L -H Expect: -H 'X-API-Key: 84-192db86cfbc5c403a46efdbec067922f' 'http://2016sv.icfpcontest.org/api/hello'

snapshot:
	curl --compressed -L -H Expect: -H 'X-API-Key: 84-192db86cfbc5c403a46efdbec067922f' 'http://2016sv.icfpcontest.org/api/snapshot/list'

# make -s snapshot | jq . > inputs/snapshot-$(my_date).json

status:
	curl --compressed -L -H Expect: -H 'X-API-Key: 84-192db86cfbc5c403a46efdbec067922f' 'http://2016sv.icfpcontest.org/api/blob/d3b221f5606083d47a1470ce875260aa0c147381'

# make -s blob | jq . > inputs/status-$(my_date).json

problem:
	curl --compressed -L -H Expect: -H 'X-API-Key: 84-192db86cfbc5c403a46efdbec067922f' 'http://2016sv.icfpcontest.org/api/blob/d5cc53ef095f64c04f2d6da3c73e5e5857e8fb74'

.PHONY: helloworld snapsnot lookup
