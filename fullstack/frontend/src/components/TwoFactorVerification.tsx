import React, { useState } from 'react';
import { 
  Box, 
  Button, 
  Heading, 
  Input, 
  Text, 
  VStack,
  HStack
} from '@chakra-ui/react';
import { TwoFactorService } from "@/client";
import useCustomToast from "../hooks/useCustomToast";

interface TwoFactorVerificationProps {
  onSuccess: (token: string) => void;
  onCancel: () => void;
  temp_token: string;
}

export const TwoFactorVerification: React.FC<TwoFactorVerificationProps> = ({ 
  onSuccess, 
  temp_token,
  onCancel 
}) => {
  const [loading, setLoading] = useState<boolean>(false);
  const [code, setCode] = useState<string>('');
  const [error, setError] = useState<string>('');
  const showToast = useCustomToast();

  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError('');
    
    try {
      const response = await TwoFactorService.verify2FaLogin({
        requestBody: {
          code,
          temp_token
        }
      });
      
      if (response.access_token) {
        showToast('Success', 'Authentication successful', 'success');
        onSuccess(response.access_token);
      } else {
        throw new Error('No access token received');
      }
    } catch (error) {
      console.error('Failed to verify 2FA code:', error);
      setError('Invalid verification code. Please try again.');
      showToast('Error', 'Invalid verification code. Please try again.', 'error');
    } finally {
      setLoading(false);
    }
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="lg" shadow="md">
      <VStack gap={4} align="stretch">
        <Heading as="h3" size="md">Two-Factor Authentication</Heading>
        
        <Text>
          Please enter the verification code from your authenticator app to complete login.
        </Text>
        
        <Box as="form" onSubmit={handleVerify}>
          <Box mb={4}>
            <Text fontWeight="medium" mb={1}>
              Verification Code
            </Text>
            <Input
              placeholder="Enter 6-digit code"
              maxLength={6}
              value={code}
              onChange={(e) => setCode(e.target.value)}
              required
              mt={1}
            />
            {error && (
              <Text color="red.500" fontSize="sm" mt={1}>{error}</Text>
            )}
          </Box>
          
          <HStack gap={4} mt={4}>
            <Button
              type="submit"
              colorScheme="blue"
              disabled={loading}
              loading={loading}
            >
              Verify
            </Button>
            <Button
              variant="outline"
              onClick={onCancel}
              disabled={loading}
            >
              Cancel
            </Button>
          </HStack>
        </Box>
      </VStack>
    </Box>
  );
};

export default TwoFactorVerification;
