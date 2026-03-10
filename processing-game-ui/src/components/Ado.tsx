import { useDrag } from 'react-dnd';
import type { Ado as AdoType } from '../types';

export const ItemTypes = {
    ADO: 'ado',
};

interface AdoProps {
    ado: AdoType;
    isDraggable: boolean;
    handleDrop: (adoId: string, processorId: string) => void;
}

export const Ado: React.FC<AdoProps> = ({ ado, isDraggable, handleDrop }) => {
    const [{ isDragging }, drag] = useDrag(() => ({
        type: ItemTypes.ADO,
        item: { id: ado.id },
        canDrag: isDraggable,
        end: (item, monitor) => {
            const dropResult = monitor.getDropResult<{ processorId: string }>();
            if (item && dropResult) {
                handleDrop(item.id, dropResult.processorId);
            }
        },
        collect: (monitor) => ({
            isDragging: !!monitor.isDragging(),
        }),
    }));

    return (
        <div
            ref={drag}
            style={{
                border: '1px solid gray',
                padding: '5px',
                margin: '5px',
                opacity: isDragging ? 0.5 : 1,
                cursor: isDraggable ? 'move' : 'default',
            }}
        >
            Ado: {ado.id.substring(0, 8)} (Size: {ado.size})
        </div>
    );
};
