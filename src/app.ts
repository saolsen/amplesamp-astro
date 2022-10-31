import { map } from 'nanostores';

export type Value = {
    type: string;
    value?: string;
    object_type?: string;
    fields?: Map<string, Value>,
}

export type TickValue = {
    tick: number;
    value: Value;
}

export type TickOutput = {
    tick: number;
    value: string;
}

export type Results = {
    last_tick: number;
    objects: Map<string, TickValue[]>,
    output: TickOutput[],
    errors: Error[]
    types: any[],
    bytecode: any[],
}

export type Error = {
    type: string;
    message: string;
    line: number;
    column: number;
}

export type Loc = {
    line: number;
    col: number;
    len: number;
}

export type App = {
    source: string,
    errors: Error[],
    results: Results | null,
    instruction: Loc | null,
};

const initialSource = `
# test program
type Organization {
    i: Int,
    number_of_employees: Int,
}
type Repository {
    i: Int,
    organization: Organization,
}
print 1
print 2
#print 1 + 2
github_org = create Organization {
    i: 1,
    number_of_employees: 1,
}
print github_org
code_repo = create Repository {
    i: 1,
    organization: github_org
}
`;

/// Store
export const appStore = map<App>({
    source: initialSource, errors: [], results: null, instruction: null
});