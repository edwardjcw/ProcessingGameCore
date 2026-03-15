import { useDrop } from 'react-dnd';
import type { Processor as ProcessorType, Status, Ado as AdoType } from '../types';
import { Ado } from './Ado';
import { ItemTypes } from './Ado';
import { Card, CardContent, Typography, Box, LinearProgress } from '@mui/material';

interface ProcessorProps {
    processor: ProcessorType;
}

const statusSize = (status: Status): number => {
    const ado = status.fields[0] as AdoType;
    switch (status.case) {
        case 'Running':
        case 'Waiting':
            return ado.size;
        default:
            return 0;
    }
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

    const usedSize = processor.ados.reduce((acc, adoStatus) => acc + statusSize(adoStatus), 0);
    const usagePercentage = (usedSize / processor.size) * 100;

    return (
        <Card
            ref={drop}
            data-processor-id={processor.id}
            className="processor"
            sx={{
                mb: 2,
                minHeight: 150,
                backgroundColor: isActive ? 'action.hover' : 'background.paper',
                border: (theme) => `1px solid ${isActive ? theme.palette.success.main : theme.palette.divider}`,
            }}
        >
            <CardContent>
                <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                    <Typography variant="h6" component="div">
                        Processor: {processor.id.substring(0, 8)}
                    </Typography>
                    <Typography variant="caption">{usedSize} / {processor.size}</Typography>
                </Box>
                <Box sx={{ display: 'flex', alignItems: 'center', mt: 1 }}>
                    <Box sx={{ width: '100%', mr: 1 }}>
                        <LinearProgress variant="determinate" value={usagePercentage} />
                    </Box>
                </Box>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, mt: 1 }}>
                    {processor.ados.map((adoStatus: Status) => (
                        <Ado key={adoStatus.fields[0].id} ado={adoStatus.fields[0]} isDraggable={false} handleDrop={handleDropDummy} />
                    ))}
                </Box>
            </CardContent>
        </Card>
    );
};
