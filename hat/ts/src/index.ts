import { exit, argv, stdout, stderr } from "process";
import { Writable } from "stream";
import path from "path";
import { compile } from "./compile";

// TODO: Parse command line arguments for bare minimum requirements

function usage() {
    write(stdout, `Usage: ${argv[0]} ${argv[1]} [OPTIONS] <SUBCOMMAND> [ARGS]`);
    write(stdout, '  OPTIONS:');
    write(stdout, '    -debug');
    write(stdout, '  SUBCOMMAND:');
    write(stdout, '    compile [OPTIONS] <FILE>');
    write(stdout, `usage: ${argv[0]} ${argv[1]} <mode> [options...]`);
}

function write(w: Writable, s: string) {
    if (!s.endsWith('\n')) {
        s += '\n';
    }
    return w.write(s);
}

interface Option {
    found: boolean;
    value: string | boolean | number;
}

type ParsedOptions = { [option: string]: Option; };
// parseOptions reads the provided args and overewrites any defaults in the options object if it finds any.
// It returns the remainin args that were not processed.
// If there an option provided in args that does not exist in options then parseOptions prints the 
// usage information and exists.
function parseGenericOptions(args: string[], options: { [option: string]: string | boolean | number; }): [string[], ParsedOptions] {
    let parsed: ParsedOptions = {};
    for (const option in options) {
        parsed[option] = {
            found: false,
            value: options[option]
        };
    }
    let i = 0;
    for (const arg of args) {
        if (!arg.startsWith('-')) {
            break;
        }
        const [option, strval = 'true'] = arg.slice(1).split('=', 2);
        let val = parsed[option];
        if (val === undefined) {
            error(`invalid option provided ${arg}`);
        }
        if (val.found) {
            error(`duplicate option provided ${arg}`);
        }
        if (typeof val.value === 'boolean') {
            switch (strval) {
                case 'true':
                    val.value = true;
                    break;
                case 'false':
                    val.value = false;
                    break;
                default:
                    error(`invalid value for option ${arg}. ${strval} is not a boolean`);
            }
        } else if (typeof val.value === 'number') {
            const num = Number(strval);
            if (isNaN(num)) {
                error(`invalid value for option ${arg}. ${strval} is not a number`);
            }
            val.value = num;
        } else if (typeof val.value === 'string') {
            val.value = strval;
        }
        val.found = true;
        i++;
    }
    return [args.slice(i), parsed];
}

function error(msg: string): never {
    usage();
    write(stderr, msg);
    exit(1);
}

interface Options {
    debug: boolean;

}
function parseOptions(args: string[]): [string[], Options] {
    const [subargs, opts] = parseGenericOptions(args, {
        debug: false,
    });
    return [subargs, {
        debug: opts.debug.value as boolean
    }];
}

interface CompileOptions {
    outputPath: string;
}

function parseCompileOptions(args: string[]): [string[], CompileOptions] {
    const [subargs, opts] = parseGenericOptions(args, {
        o: ''
    });
    return [subargs, {
        outputPath: opts.o.value as string
    }];
}

function runCompile(options: Options, args: string[]) {
    const [subargs, cOptions] = parseCompileOptions(args);
    if (subargs.length === 0) {
        error('no file provided');
    } else if (subargs.length > 1) {
        error(`too many arguments provided ${subargs}`);
    }
    const filePath = subargs[0];
    let outputPath = cOptions.outputPath;
    if (outputPath === '') {
        outputPath = path.basename(filePath, path.extname(filePath));
    }
    compile(filePath, outputPath);
}

function main() {
    let args = argv.slice(2);
    let options: Options;
    [args, options] = parseOptions(args);
    if (args.length === 0) {
        usage();
        stderr.write('no subcommand provided\n');
        exit(1);
    }
    switch (args[0]) {
        case 'help':
            usage();
            exit(0);
        case 'compile':
            runCompile(options, args);
            break;
        default:
            stderr.write(`invalid compiler mode ${args[0]}\n`);
            usage();
            exit(1);
    }
}

if (require.main === module) {
    main();
}
