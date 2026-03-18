import { useDrag } from 'react-dnd';
import type { Ado as AdoType } from '../types';
import { Paper, Typography, Box, LinearProgress } from '@mui/material';

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

    const progress = (ado.adone / ado.size) * 100;

    return (
        <Paper
            ref={drag}
            elevation={3}
            data-ado-id={ado.id}
            className="ado"
            sx={{
                padding: 1,
                margin: 0.5,
                opacity: isDragging ? 0.5 : 1,
                cursor: isDraggable ? 'move' : 'default',
                textAlign: 'center',
                width: '120px'
            }}
        >
            <Typography variant="body2">Ado: {ado.id.substring(0, 4)}</Typography>
            <Box sx={{ width: '100%', mt: 1 }}>
                <LinearProgress variant="determinate" value={progress} />
            </Box>
            <Typography variant="caption">{ado.adone} / {ado.size}</Typography>
        </Paper>
    );
};

