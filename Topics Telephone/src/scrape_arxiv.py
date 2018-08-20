import arxiv

def to_slug(title):
    # Remove special characters
    filename = ''.join(c if c.isalnum() else '_' for c in title)
    # delete duplicate underscores
    filename = '_'.join(list(filter(None, filename.split('_'))))
    return filename

cats = ["cs.AI", "cs.AR", "cs.CC", "cs.CE", "cs.CG", "cs.CL", "cs.CR", "cs.CV",
		"cs.CY", "cs.DB", "cs.DC", "cs.DL", "cs.DM", "cs.DS", "cs.ET", "cs.FL",
		"cs.GL", "cs.GR", "cs.GT", "cs.HC", "cs.IR", "cs.IT", "cs.LG", "cs.LO",
		"cs.MA", "cs.MM", "cs.MS", "cs.NA", "cs.NE", "cs.NI", "cs.OH", "cs.OS",
		"cs.PF", "cs.PL", "cs.RO", "cs.SC", "cs.SD", "cs.SE", "cs.SI", "cs.SY"]

for cat in cats:

	print cat

	docs = arxiv.query(search_query=("cat:"+cat), max_results=2000)

	print "Found", len(docs), "articles"

	for doc in docs:

		# slugify name for filename
		filename = to_slug(doc["title"])

		with open("arxiv_abstracts/"+filename, 'w') as f:
			# write abstract to file
			f.write(doc['summary'])


