
NC=IOTC-2016-DATASETS-NCDB_0.zip
CE=IOTC-2016-DATASETS-CE_All_Gears.zip

all: get unzip

get:
	wget http://iotc.org/sites/default/files/documents/2016/08/$(NC)
	wget http://iotc.org/sites/default/files/documents/2016/08/$(CE)

unzip:
	unzip $(NC)
	unzip $(CE)

clean:
	rm -f *.csv *.xlsx
