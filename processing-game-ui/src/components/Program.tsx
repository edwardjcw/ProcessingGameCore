import type { Program as ProgramType } from '../types';
import { Ado } from './Ado';
import { Card, CardContent, Typography, Box } from '@mui/material';

interface ProgramProps {
    program: ProgramType;
    handleDrop: (adoId: string, processorId: string) => void;
}

export const Program: React.FC<ProgramProps> = ({ program, handleDrop }) => {
    return (
        <Card sx={{ mb: 2 }}>
            <CardContent>
                <Typography variant="h6" component="div">
                    Program: {program.id.substring(0, 8)}
                </Typography>
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, mt: 1 }}>
                    {Object.values(program.readyAdos).map((adoStatus) => (
                        <Ado key={adoStatus.fields[0].id} ado={adoStatus.fields[0]} isDraggable={true} handleDrop={handleDrop} />
                    ))}
                </Box>
            </CardContent>
        </Card>
    );
};
