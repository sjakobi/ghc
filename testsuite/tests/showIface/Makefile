TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

Orphans:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Orphans.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface Orphans.hi | grep -E '^(instance |family instance |"myrule)' | grep -v 'family instance modules:'

DocsInHiFile0:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c DocsInHiFile.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface DocsInHiFile.hi | grep -A 4 'docs:'

DocsInHiFile1:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -haddock DocsInHiFile.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface DocsInHiFile.hi | grep -A 200 'docs:'

NoExportList:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -haddock NoExportList.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface NoExportList.hi | grep -A 100 'docs:'

PragmaDocs:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -haddock PragmaDocs.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface PragmaDocs.hi | grep -A 100 'Warnings:'

HaddockOpts:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -haddock HaddockOpts.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface HaddockOpts.hi | grep -A 100 'docs:'

LanguageExts:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -haddock LanguageExts.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface LanguageExts.hi | grep -A 100 'docs:'
