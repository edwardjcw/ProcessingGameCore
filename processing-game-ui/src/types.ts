export interface Ado {
    id: string;
    size: number;
    adone: number;
    programId: string;
}

export type Status = 
    | { case: 'Ready'; fields: [Ado] }
    | { case: 'Running'; fields: [Ado] }
    | { case: 'Waiting'; fields: [Ado] }
    | { case: 'Done'; fields: [Ado] };

export interface Program {
    id: string;
    size: number;
    readyAdos: Record<string, Status>;
}

export interface Processor {
    id: string;
    size: number;
    ados: Status[];
    power: number;
}

export interface Environment {
    programs: Record<string, Program>;
    processors: Record<string, Processor>;
    ticks: number;
}
