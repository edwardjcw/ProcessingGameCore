import { useDrop } from 'react-dnd';
import type { Processor as ProcessorType, Status } from '../types';
import { Ado } from './Ado';
import { ItemTypes } from './Ado';

interface ProcessorProps {
    processor: ProcessorType;
}

// Dummy handler for non-draggable ados
const handleDropDummy = () => {};

export const Processor: React.FC<ProcessorProps> = ({ processor }) => {
    const [{ canDrop, isOver }, drop] = useDrop(() => ({
        accept: ItemTypes.ADO,
        drop: () => ({ processorId: processor.id }),
        collect: (monitor) => ({
            isOver: monitor.isOver(),
            canDrop: monitor.canDrop(),
        }),
    }));

    const isActive = canDrop && isOver;

    return (
        <div
            ref={drop}
            style={{
                border: `1px solid ${isActive ? 'green' : 'black'}`,
                padding: '10px',
                margin: '10px',
                minHeight: '100px',
                backgroundColor: isActive ? '#ddffdd' : 'transparent',
            }}
        >
            <h4>Processor: {processor.id.substring(0, 8)}</h4>
            <div>
                {processor.ados.map((adoStatus: Status) => (
                    <Ado key={adoStatus.fields[0].id} ado={adoStatus.fields[0]} isDraggable={false} handleDrop={handleDropDummy} />
                ))}
            </div>
        </div>
    );
};
