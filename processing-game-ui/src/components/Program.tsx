import type { Program as ProgramType } from '../types';
import { Ado } from './Ado';

interface ProgramProps {
    program: ProgramType;
    handleDrop: (adoId: string, processorId: string) => void;
}

export const Program: React.FC<ProgramProps> = ({ program, handleDrop }) => {
    return (
        <div style={{ border: '1px solid black', padding: '10px', margin: '10px' }}>
            <h4>Program: {program.id.substring(0, 8)}</h4>
            <div>
                {Object.values(program.readyAdos).map((adoStatus) => (
                    <Ado key={adoStatus.fields[0].id} ado={adoStatus.fields[0]} isDraggable={true} handleDrop={handleDrop} />
                ))}
            </div>
        </div>
    );
};
