from __future__ import print_function
import wikipedia
import datetime, os, sys
from time import time

wikipedia.set_rate_limiting(rate_limit=True, min_wait=datetime.timedelta(0, 0, 1e6))
wikipedia.set_lang("en")

# Nicely formatted time string
def hms_string(sec_elapsed):
    h = int(sec_elapsed / (60 * 60))
    m = int((sec_elapsed % (60 * 60)) / 60)
    s = sec_elapsed % 60
    return "{}:{:>02}:{:>05.2f}".format(h, m, s)

cat_orig = ["Artificial intelligence", "Companies", "Computer architecture",
			  "Computer model", "Computer engineering", "Data", "Free software",
			  "Human-computer interaction", "Internet", "Mobile web", "Networks",
			  "Operating systems", "Programming", "Software", "Automation",
			  "Unsolved problems in computer science", "Computer security",
			  "Computing and society", "Embedded systems", "Multimedia",
			  "Languages", "Information technology", "Information systems",
			  "Platforms", "Product lifecycle management", "Real-time computing",
			  "Software engineering", "Computer science", "Classes of computers",
			  "Machine Learning", "Statistics", "Mathematics", "Supercomputer",
			  "Quantum physics", "Quantum computer", "compiler", 
			  "Central processing unit", "Graphics processing unit",
			  "Personal Computer", "Video Games", "World Wide Web", "Internet policy",
			  "Graphical user interface", "Laptops", "Desktops", "Startup", 
			  "silicon valley", "Google", "Facebook", "Microsoft", "Amazon",
			  "Apple", "algorithm", "programming languages", "Computer graphics",
			  "logistic regression", "neural networks", "computer ethics", "hacking",
			  "internet regulation", "data regulation", "data science", "cryptography",
			  "databases", "distributed systems", "visualization", "data structures",
			  "academia", "alan turing", "turing machine", "computer science education",
			  "computer science conferences", "correlation", "regression", 
			  "algorithmic bias", "gender gap", "internet policy", "linear algebra",
			  "cognitive science", "linguistics", "mathematics", "physics", "biology",
			  "logic", "health technologies", "alonzo church", "intel", 
			  "random access memory", "read only memory", "arduino", "raspberry pi",
			  "Macbook", "surface pro", "iphone 8", "android", "galaxy s8", 
			  "brian kernighan", "Grace Hopper", "Steve Jobs", "Jeff Bezos",
			  "david knuth", "fortran", "microprocessor", "functional programming",
			  "graph theory", "machine translation", "computer vision", 
			  "computational geometry", "analytic combinatorics", "nonstandard computation",
			  "computational complexity", "probabilistic algorithms", "biometrics", 
			  "client/server", "Green IT", "Lasers", "Measurement",
			  "Nanotechnology", "pervasive computing", "supercomputing", "virtualization",
			  "vocations", "ICANN", "IANA", "fiber optics", "solid state", "hard drive",
			  "cache", "broadband", "attenuation", "dark web", "clean computing", 
			  "electronic waste", "obsolete", "hibernate mode", "power management",
			  "cloud computing", "bitcoin", "blockchain", "containerization", "ipad",
			  "heartbleed", "verification", "bioidentification", "chat bot", 
			  "natural language processing", "bag of words", "term frequency",
			  "duty cycle", "resistor", "capacitor", "byte", "teraflop", "sampling rate",
			  "carbon nanotube", "nanosecond", "storage virtualization", 
			  "network virtualization", "cryptocurrency", "array", "packet", "pixel",
			  "voxel", "backup", "recovery", "raytracing", "raycasting", "rasterize",
			  "animation", "wordnet", "seam carving", "percolation", "autocomplete",
			  "kd tree", "symbol table", "buffer overflow", "heap manager",
			  "Culture", "Humanities", "Classics", "Critical theory", "Cultural anthropology",
			  "Folklore", "Food culture", "Food and drink", "Languages", "Literature", 
			  "Museology", "Mythology", "Philosophy", "Popular culture", "Science and culture",
			  "Traditions", "Arts and Entertainment", "Arts and crafts", "Celebrity",
			  "Censorship in the arts", "Festivals", "Humor", "Literature", "Museums", "Parties", 
			  "Poetry", "Performing arts", "Circus", "Dance", "Film", "Music", "Opera", "Storytelling",
			  "Theatre", "Visual Arts", "architecture", "comics", "crafts", "design", "drawing",
			  "film (animation)", "new media art", "painting", "photography", "sculpture"]

categories = ["Games and toys", "board games", "card games", "dolls", "puppetry", "puzzles",
			  "role-playing games", "video games", "Sports and recreation", "air sports", 
			  "american football", "association football", "auto racing", "baseball", "basketball",
			  "boating", "boxing", "canoeing", "cricket", "cycling", "exercise", "fishing", "golf",
			  "gymnastics", "hobbies", "horse racing", "ice hockey", "Lacrosse", "Olympic games",
			  "rugby league", "rugby union", "sailing", "skiing", "swimming", "tennis", "track and field",
			  "walking trails", "whitewater sports", "mass media", "broadcasting", "unabomber", "FBI",
			  "broadcasting", "film", "magazines", "newspapers", "publications", "publishing", 
			  "television", "radio", "earth", "world", "bodies of water", "cities", "communities",
			  "contintents", "countries", "deserts", "lakes", "landforms", "mountiains", "navigations",
			  "oceans", "populated places", "protected areas", "regions", "rivers", "subterranea", 
			  "territories", "towns", "villages", "Self care", "health promotion", "life extension",
			  "prevention", "sexual health", "sleep", "skin care", "Nutrition", "dietary supplements",
			  "dietetics", "nutrients", "aerobics", "calisthenics", "hiking", "pilates", "running",
			  "tai chi", "yoga", "hygiene", "positive psychology", "psychotherapy", "forensic linguistics",
			  "health standards", "hospitals", "pharmaceutical industry", "safety", "diseases",
			  "epidemiology", 'midwifery', 'nursing', 'optometry', 'public health', 'historiography',
			  'Science history', 'religion history', 'african history', 'asian history', 'european history',
			  'middle eastern history', 'american history', 'south american history', 'australian history',
			  'equations', 'proofs', 'theorems', 'numbers', 'arithmetic', 'algebra', 'geometry', 'calculus',
			  'deductive reasoning', 'inductive reasoning', 'logical fallacies', 'metalogic', 'philosophy',
			  'analysis of variance', 'bayesian statistics', 'categorical data', 'covariance and correlation',
			  'decision theory', 'multivariate statistics', 'parametric', 'nonparametric', 'sampling',
			  'stochastic', 'biology', 'ecology', 'neuroscience', 'zoology', 'atmospheric sciences',
			  'geography', 'geology', 'oceanography', 'animals', 'environment', 'pollution', 'astronomy',
			  'chemistry', 'climate', 'space', 'universe', 'children', 'heads of state', 'men', 'women',
			  'ethnicity', 'nationality', 'occupation', 'death', 'murder', 'crime', 'serial killer',
			  'slaves', 'civil war', 'world war', 'racism', 'victims', 'literature', 'aesthetics',
			  'epistemology', 'ethics', 'social philosophy', 'catholocism', 'judaism', 'islam', 'hindu',
			  'sikh', 'christian', 'jesus', 'emotion', 'atheism', 'memory', 'allah', 'bible', 'god',
			  'prayer', 'mythology', 'animism', 'gnosticism', 'ritual', 'globalization', 'economics',
			  'macroeconomics', 'microeconomics', 'econometrics', 'inference', 'causal inference',
			  'agriculture', 'agronomy', 'earthquake engineering', 'wildfire', 'flood', 'blizzard',
			  'thunderstorm', "tornado", 'forecast', 'meteorology', 'nuclear', 'weapon of mass destruction',
			  'cold war', '9/11']
t0 = time()
i = 0
for cat in categories:
	print(cat)

	# Get top 100 title results related to query term in categories
	results = wikipedia.search(cat, results=400, suggestion=False)

	# Attempt to match each title to a page object
	for res in results:
		try:
			page = wikipedia.page(title=res)
		except wikipedia.exceptions.DisambiguationError as e:
			try:
				page = wikipedia.page(title=e.options[0])
			except wikipedia.exceptions.DisambiguationError as f:
				continue
			except wikipedia.exceptions.PageError as g:
				continue
			except wikipedia.exceptions.WikipediaException as h:
				continue
		except wikipedia.exceptions.PageError as e:
			continue
		except wikipedia.exceptions.WikipediaException as h:
			continue

		# Remove "/" from the article titles for file naming purposes
		res = " ".join(res.split("/"))
		file = "wiki_corpus/" + "_".join(res.split()) + ".txt"

		# Avoid copying the same article, write just the page content
		if not os.path.isfile(file):
			with open(file, 'w') as f:
				f.write(page.content.encode("ascii", errors="ignore"))
				i += 1

			print("Articles processed: {}".format(i), end='\r')
			sys.stdout.flush()

print("Total articles processed: {}".format(i))
print("Elapsed time: {}".format(hms_string(time() - t0)))


