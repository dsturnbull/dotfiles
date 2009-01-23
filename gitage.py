import os, re, datetime

try:
    import vim
except ImportError, e:
    # stub for testing.
    class vim:
        @staticmethod
        def command(cmd):
            print cmd

NOW = datetime.datetime.now()

def cwd():
    return os.getcwd()

def timedelta_to_i(td):
    return (td.days * 24 * 60 * 60) + td.seconds

def line_to_anno(line):
    not_committed = re.match("^00000000.*", line)
    match = re.match("^(\w+)\s+\((\s*.+)\s+(\d{4}.*\+\d{4})\s+(\d+)\)(.*)", line)

    if not_committed:
        return { 'date': NOW }
    elif match:
        r, author, datestr, n, line = map(lambda s: s.strip(), match.groups())
        datebits = re.split("[+:\-\s]", datestr)
        date = datetime.datetime(*[int(bit) for bit in datebits if bit])
        return {
            'rev': r,
            'author': author,
            'date': date,
            'lineno': n,
            'line': line,
        }

def annotate(filename):
    out = os.popen("git --git-dir=%s --work-tree=%s annotate %s" %
            (os.path.join(cwd(), '.git'), cwd(), filename))
    return map(line_to_anno, out)

def anno_colors(annos):
    dates = [ a['date'] for a in annos ]

    t0 = min(dates)
    time_range = timedelta_to_i(max(dates) - t0) or 1 # if every line same age

    gradient = 255.0 / time_range
    def f(t):
        return gradient * timedelta_to_i(t - t0)

    return [ "#%02x0000" % f(t) for t in dates ]

def main(filename):

    annos = annotate(filename)
    colors = anno_colors(annos)

    # create hi groups
    for c in set(colors):
        cmd = "highlight git_age_%s guibg=%s" % (c[1:], c)
        vim.command(cmd)

    # set syn.
    for i, c in enumerate(colors):
        cmd = "syntax match git_age_%s '\\%%%dl.*'" % (c[1:], i+1)
        vim.command(cmd)

if __name__ == '__main__':
    try:
        import vim
        main(vim.current.buffer.name)
    except ImportError, e:
        import sys
        main(sys.argv[-1])

